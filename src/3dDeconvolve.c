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
  Program to calculate the deconvolution of a measurement 3D+time dataset
  with a specified input stimulus time series.  This program will also
  perform multiple linear regression using multiple input stimulus time
  series. Output consists of an AFNI 'bucket' type dataset containing the
  least squares estimates of the linear regression coefficients, t-statistics
  for significance of the coefficients, partial F-statistics for significance
  of the individual input stimuli, and the F-statistic for significance of
  the overall regression.  Additional output consists of a 3D+time dataset
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
           and the residual error time series (-errts) to 3D+time datasets.
  Date:    22 November 1999

  Mod:     Added option to perform analysis on a single (fMRI) measurement
           time series instead of a 3D+time dataset (-input1D).
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

  Mod:     Added -concat option for analysis of a concatenated 3D+time dataset.
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
/* Decide on the inclusion of mri_matrix.h before including mri_lib.h
   The latter now ends up including mri_matrix.h in a roundabout way
   via suma_utils.h                                   ZSS Nov. 21 2014   */
#ifndef FLOATIZE
# include "matrix.h"          /* double precision */
# define MTYPE    double
# define NI_MTYPE NI_DOUBLE
# define QEPS     1.e-6
# define MPAIR    double_pair
#else
# include "matrix_f.h"        /* single precision */
# define MTYPE    float
# define NI_MTYPE NI_FLOAT
# define QEPS     1.e-4
# define MPAIR    float_pair
#endif

#include "mrilib.h"           /* Keep after decision about matrix.h inclusion
                                                      ZSS  Nov. 21 2014*/
#include "coxplot.h"

#ifdef isfinite
# define IS_GOOD_FLOAT(x) isfinite(x)
#else
# define IS_GOOD_FLOAT(x) finite(x)
# define isfinite finite
#endif

/* 01 Feb 2011 -- minor adjustments */

#define myceil(x)   ceil((x)-0.00005)
#define myfloor(x) floor((x)+0.00005)

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

  static int         virtu_mrv  = 0   ; /* use a virtual vectim? [26 Dec 2012] */
  static char       *fname_mrv  = NULL;
  static MRI_vectim *inset_mrv  = NULL;

#else   /* can't use multiple processes */

# define proc_numjob 1   /* flag that only 1 process is in use */
# define proc_ind    0   /* index of THIS job */

# define virtu_mrv   0   /* 26 Dec 2012 */
# define fname_mrv   NULL
# define inset_mrv   NULL

#endif

  static int proc_use_jobs      = 0   ; /* jobs opt given - 2003.08.15[rickr] */

/*---------------------------------------------------------------------------*/


#define MEM_MESSAGE                                                      \
 do{ long long val = MCW_MALLOC_total ;                                  \
     if( val > 0 )                                                       \
       INFO_message("current memory malloc-ated = %s bytes (about %s)" , \
                    commaized_integer_string(val) ,                      \
                    approximate_number_string((double)val) ) ;           \
 } while(0)

static int aa_len_AA ;
#undef  ALEN
#define ALEN(na) (aa_len_AA=strlen(argv[na])+16 , MAX(aa_len_AA,THD_MAX_NAME))

/*------------ prototypes for routines far below (RWCox) ------------------*/

void JPEG_matrix_gray( matrix X, char *fname );        /* save X matrix to JPEG */
void ONED_matrix_save( matrix X, char *fname,          /* save X matrix to .1D */
                       void *,int,int *, matrix *Xf ,
                       int nbl, int *bl, void *gst, void *sst ) ;

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

static int dofsub=0 ;

float baseline_mean( vector coef ) ;    /* compute mean of baseline stuff */

static int xrestore = 0 ;                           /* globals for -xrestore */
static char *xrestore_filename = NULL ;
static int NumTimePoints=0 , NumRegressors=0 ;

static int verb = 1 ;
static char *commandline = NULL ;

static int goforit = 0 ;  /* 07 Mar 2007 */
static int badlev  = 0 ;
static int floatout= 1 ;  /* 13 Mar 2007 ; 15 Jul 2010: now defaults on */

static int dont_do_satcheck = 1 ; /* 23 Dec 2011 */

/* include other dset types for float output   2 Apr 2009 [rickr] */
#define CHECK_NEEDS_FLOATS(fn)                                         \
 do{ if( !floatout &&                                                  \
         ( strstr((fn),".nii") || strstr((fn),".niml.dset") ||         \
           strstr((fn),".gii") ) ){                                    \
       WARNING_message(                                                \
        "output prefix is '%s' ==> forcing '-float'" , (fn)) ;         \
       floatout = 2 ;                                                  \
   }} while(0)

static int allzero_OK = 0 ;  /* 30 May 2007 */

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

static int do_FDR = 1 ;                 /* 23 Jan 2008 */

static byte *gmask = NULL ;             /* 03 Feb 2009 -- global mask array */

static bytevec *statmask = NULL ;       /* 15 Jul 2010 -- ditto */
static char    *statmask_name = NULL ;

/** for -stim_times_FSL [15 Nov 2013] **/

MRI_IMAGE * convert_FSL_to_fvect( MRI_IMAGE *fslim , int do_amp1 ) ;

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

/*! Macro to change the parameters in a basis_func structure **/

#define basis_func_setparn(bf,nn,pv)        \
 do{      if( (nn) <= 0 ) (bf).a = (pv) ;   \
     else if( (nn) == 1 ) (bf).b = (pv) ;   \
     else                 (bf).c = (pv) ; } while(0)

/** Macros to define the meaning of the IRF times [08 Mar 2007] ('type') **/

#define BASIS_SINGLE         1   /* same IRF for all times */
#define BASIS_MODULATED_MONO 2   /* amplitude modulated IRF */
#define BASIS_MODULATED_PAIR 3   /* mean and delta amplitude modulated IRF */
#define BASIS_MODULATED_INDV 9   /* each stim gets own basis func [16 Jul 2007] */

#define BASIS_MAX_VDIM      21   /* max number dimens in -stim_times_AM2 */
#define BASIS_MAX_VFUN       3   /* max number of nonlinear parameters */

#define BASIS_MIN_DURATION    0.1f  /* min stimulus duration in seconds */
#define BASIS_MAX_DURATION  999.0f  /* max stimulus duration in seconds */

static float stime_sub = 0.0f ;     /* 24 Mar 2009: stimulus time subtractor */
static float stime_fac = 1.0f ;     /*              stimulus time scale fac */

/*! A whole set of basis functions (generated by -stim_times 3rd argument). */

typedef struct {
  int type , nfunc , nparm , pbot , timetype , vfun,vmod,vdim , no_iresp ;
  float tbot,ttop ;
  basis_func *bfunc ;
  char *name , *symfun , *option ;
  float *modsub ;                   /* 12 Jul 2012: modulation subtractors */
} basis_expansion ;

/** Extra baseline orts **/

static MRI_IMARR *ortar = NULL ;  /* 02 Dec 2011 */

/** Prototypes for some basis expansion functions appearing (much) later. **/

basis_expansion * basis_parser( char *sym ) ;
float basis_evaluation( basis_expansion *be , float *wt , float x ) ;
void basis_write_iresp( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float **wtar , char *output_filename ) ;
void basis_write_iresp_1D( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float **wtar , char *output_filename ) ;
void basis_write_sresp( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float *mse ,
                        int pbot, matrix cvar, char *output_filename ) ;
void basis_write_sresp_1D( int argc , char *argv[] ,
                        struct DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float *mse ,
                        int pbot, matrix cvar, char *output_filename ) ;

/** global variables for stimulus basis expansions **/

static basis_expansion **basis_stim  = NULL ; /* equations for response model */
static MRI_IMAGE       **basis_times = NULL ; /* times for each response      */
static MRI_IMAGE       **basis_vect  = NULL ; /* vectors generated from above */
static int               basis_nstim = 0    ; /* number of basis_stim entries */
static int               basis_nused = 0    ; /* number that are not NULL */
static float             basis_TR    = 1.0f ; /* data time step in seconds */
static int               basis_count = 0    ; /* any -stim_times inputs? */
static float             basis_dtout = 0.0f ; /* IRF time step in seconds */
static float             irc_dt      = 0.0f ;

static int               basis_need_mse = 0 ; /* need MSE volume */

static float             basis_normall  = 0.0f ; /* 28 Apr 2005 */

#define GUESS_TIMES  0
#define GLOBAL_TIMES 1
#define LOCAL_TIMES  2
static int               basis_timetype = GUESS_TIMES ;  /* 16 Nov 2007 */

#define basis_filler 3.e+33 /* filler in basis_times for missing entries */
#define big_time     1.e+9  /* a very long time (31+ years) */

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

static char index_prefix = '#' ;  /* 11 Jun 2019 */

#define COLUMN_LABEL(n) ( ( (n) < ncoldat ) ? coldat[n].name : "unknown" )

#undef  USE_OLD_LABELS

#define FIX_CONFLICTS   /* 23 Mar 2007 */

/*---------------------------------------------------------------------------*/
/* 31 July 2008: stuff to store all GLT and stim label/column correspondence */

typedef struct {
  int     glt_num ;
  char  **glt_lab ;
  matrix *glt_mat ;
} glt_stuff ;

typedef struct {
   int nstim ;
   int *cbot , *ctop ;
   char **label ;
} stimlabel_stuff ;

static glt_stuff       *GLT_stuff       = NULL ;
static stimlabel_stuff *STIMLABEL_stuff = NULL ;

/*---------------------------------------------------------------------------*/

#include "Deconvolve.c"

/*---------------------------------------------------------------------------*/

typedef struct DC_options
{
  int nxyz;                /* number of voxels in the input dataset */
  int nt;                  /* number of input 3D+time dataset time points */
  int NFirst;              /* first image from input 3D+time dataset to use */
  int NLast;               /* last image from input 3D+time dataset to use */
  int N;                   /* number of usable data points from input data */
  int polort;              /* degree of polynomial for baseline model */
  float rms_min;           /* minimum rms error to reject reduced model */
  int quiet;               /* flag to suppress screen output */
  int progress;            /* print periodic progress reports */
  float fdisp;             /* minimum f-statistic for display */
  char * input_filename;   /* input 3D+time dataset */
  char * mask_filename;    /* input mask dataset */
  char * input1D_filename; /* input fMRI measurement time series */
  float  input1D_TR;       /* TR for input 1D time series */
  float  force_TR;         /* force use of this TR for 3D time series */
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
  char ** iresp_filename;  /* impulse response 3D+time output */
  char ** sresp_filename;  /* std. dev. 3D+time output */
  char * fitts_filename;   /* fitted time series 3D+time output */
  char * errts_filename;   /* error time series 3D+time output */
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
  char *x1D_unc ;       /* 25 Jun 2007 - uncensored matrix */
  char *x1D_regcen ;    /* 16 Aug 2019 - censoring via regression */
  int x1D_stop ;        /* 28 Jun 2007 */

  int automask ;        /* flag to do automasking [15 Apr 2005] */

  int   nodata_NT ;     /* optional values after -nodata [27 Apr 2005] */
  float nodata_TR ;

  int tcat_noblock ;    /* 06 Jan 2011 */

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

void display_help_menu(int detail)
{
  identify_software();

  printf ("\n"
   "------------------------------------------------------------------------\n"
   "-----                 DESCRIPTION and PROLEGOMENON                 -----\n"
   "------------------------------------------------------------------------\n"
   "Program to calculate the deconvolution of a measurement 3D+time dataset \n"
   "with a specified input stimulus time series.  This program can also     \n"
   "perform multiple linear regression using multiple input stimulus time   \n"
   "series. Output consists of an AFNI 'bucket' type dataset containing     \n"
   "(for each voxel)                                                        \n"
   " * the least squares estimates of the linear regression coefficients    \n"
   " * t-statistics for significance of the coefficients                    \n"
   " * partial F-statistics for significance of individual input stimuli    \n"
   " * the F-statistic for significance of the overall regression model     \n"
   "The program can optionally output extra datasets containing             \n"
   " * the estimated impulse response function                              \n"
   " * the fitted model and error (residual) time series                    \n"
   "------------------------------------------------------------------------\n"
   "* Program 3dDeconvolve does Ordinary Least Squares (OLSQ) regression.   \n"
   "* Program 3dREMLfit can be used to do Generalized Least Squares (GLSQ)  \n"
   "  regression (AKA 'pre-whitened' least squares) combined with REML      \n"
   "  estimation of an ARMA(1,1) temporal correlation structure:            \n"
   "   https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dREMLfit.html   \n"
   "* The input to 3dREMLfit is the .xmat.1D matrix file output by          \n"
   "  3dDeconvolve, which also writes a 3dREMLfit command line to a file    \n"
   "  to make it relatively easy to use the latter program.                 \n"
   "* 3dREMLfit also allows for voxel-specific regressors, unlike           \n"
   "  3dDeconvolve. This feature is used with the '-fanaticor' option       \n"
   "  to afni_proc.py, for example.                                         \n"
   "* Nonlinear time series model fitting can be done with program 3dNLfim: \n"
   "   https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dNLfim.html     \n"
   "* Preprocessing of the time series input can be done with various AFNI  \n"
   "  programs, or with the 'uber-script' afni_proc.py:                     \n"
   "   https://afni.nimh.nih.gov/pub/dist/doc/program_help/afni_proc.py.html\n"
   "------------------------------------------------------------------------\n"
   "------------------------------------------------------------------------\n"
   "****  The recommended way to use 3dDeconvolve is via afni_proc.py,  ****\n"
   "****  which will pre-process the data, and also provide some useful ****\n"
   "****  diagnostic tools/outputs for assessing the data's quality.    ****\n"
   "****  It can also run 3dREMLfit for you 'at no extra charge'.       ****\n"
   "****  [However, it will not wax your car or wash your windows.]     ****\n"
   "------------------------------------------------------------------------\n"
   "------------------------------------------------------------------------\n"
   "Consider the time series model  Z(t) = K(t)*S(t) + baseline + noise,    \n"
   "where Z(t) = data                                                       \n"
   "      K(t) = kernel (e.g., hemodynamic response function or HRF)        \n"
   "      S(t) = stimulus time series                                       \n"
   "  baseline = constant, drift, etc. [regressors of no interest]          \n"
   "     and * = convolution                                                \n"
   "Then 3dDeconvolve solves for K(t) given S(t).  If you want to process   \n"
   "the reverse problem and solve for S(t) given the kernel K(t), use the   \n"
   "program 3dTfitter with the '-FALTUNG' option.  The difference between   \n"
   "the two cases is that K(t) is presumed to be causal and have limited    \n"
   "support, whereas S(t) is a full-length time series.  Note that program  \n"
   "3dTfitter does not have all the capabilities of 3dDeconvolve for        \n"
   "calculating output statistics; on the other hand, 3dTfitter can solve   \n"
   "a deconvolution problem (in either direction) with L1 or L2 regression, \n"
   "and with sign constraints on the computed values (e.g., requiring that  \n"
   "the output S(t) or K(t) be non-negative):                               \n"
   " https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dTfitter.html     \n"
   "------------------------------------------------------------------------\n"
   "The 'baseline model' in 3dDeconvolve (and 3dREMLfit) does not mean just \n"
   "a constant (mean) level of the signal, or even just the slow drifts that\n"
   "happen in FMRI time series.  'Baseline' here also means the model that  \n"
   "forms the null hypothesis.  The Full_Fstat result is the F-statistic    \n"
   "of the full model (all regressors) vs. the baseline model.  Thus, it    \n"
   "it common to include irregular time series, such as estimated motion    \n"
   "parameters, in the baseline model via the -stim_file/-stim_base options,\n"
   "or by using the -ortvec option (to include multiple regressors at once).\n"
   "Thus, the 'baseline model' is really the 'null hypothesis model'.       \n"
   "------------------------------------------------------------------------\n"
   "It is VERY important to realize that statistics (F, t, R^2) computed in \n"
   "3dDeconvolve are MARGINAL (or partial) statistics.  For example, the    \n"
   "t-statistic for a single beta coefficient measures the significance of  \n"
   "that beta value against the regression model where ONLY that one column \n"
   "of the matrix is removed; that is, the null hypothesis for that         \n"
   "t-statistic is the full regression model minus just that single         \n"
   "regressor.  Similarly, the F-statistic for a set of regressors measures \n"
   "the significance of that set of regressors (eg, a set of TENT functions)\n"
   "against the full model with just that set of regressors removed.  If    \n"
   "this explanation or its consequences are unclear, you need to consult   \n"
   "with a statistician, or with the AFNI message board guru entities       \n"
   "(when they can be lured down from the peak of Mt Taniquetil or Kailash).\n"
   "------------------------------------------------------------------------\n"
   "Regression Programs in the AFNI Package:                                \n"
   "* At its core, 3dDeconvolve solves a linear regression problem z = X b  \n"
   "  for the parameter vector b, given the data vector z in each voxel, and\n"
   "  given the SAME matrix X in each voxel.  The solution is calculated in \n"
   "  the Ordinary Least Squares (OLSQ) sense.                              \n"
   "* Program 3dREMLfit does something similar, but allows for ARMA(1,1)    \n"
   "  serial correlation in the data, so the solution method is called      \n"
   "  Generalized Least Squares (GLSQ).                                     \n"
   "* If you want to solve a problem where some of the matrix columns in X  \n"
   "  (the regressors) are different in different voxels (spatially variable),\n"
   "  then use program 3dTfitter, which uses OLSQ, or used 3dREMLfit.       \n"
   "* 3dTfitter can also use L1 and LASSO regression, instead of OLSQ; if you\n"
   "  want to use such 'robust' fitting methods, this program is your friend.\n"
   "  It can also impose sign constraints (positivity or negativity) on the \n"
   "  parameters b, and can (as mentioned above) do deconvolution.          \n"
   "* 3dBandpass and 3dTproject can do a sequence of 'time series cleanup'  \n"
   "  operations, including 'regressing out' (via OLSQ) a set of nuisance   \n"
   "  vectors (columns).                                                    \n"
   "* 3dLSS can be used to solve -stim_times_IM systems using an alternative\n"
   "  linear technique that gives biased results, but with smaller variance.\n"
   "------------------------------------------------------------------------\n"
  ) ;
  printf("\n"
    "Usage Details:                                                         \n"
    PROGRAM_NAME " command-line-arguments ...\n"
    "                                                                       \n"
    "**** Input data and control options ****                               \n"
    "\n"
    "-input fname         fname = filename of 3D+time input dataset         \n"
    "                       [more than  one filename  can  be  given]       \n"
    "                       [here,   and  these  datasets  will   be]       \n"
    "                       [auto-catenated in time; if you do this,]       \n"
    "                       ['-concat' is not needed and is ignored.]       \n"
    "                  ** You can input a 1D time series file here,         \n"
    "                     but the time axis should run along the            \n"
    "                     ROW direction, not the COLUMN direction as        \n"
    "                     in the -input1D option.  You can automatically    \n"
    "                     transpose a 1D file on input using the \\'        \n"
    "                     operator at the end of the filename, as in        \n"
    "                       -input fred.1D\\'                               \n"
    "                   * This is the only way to use 3dDeconvolve          \n"
    "                     with a multi-column 1D time series file.          \n"
    "                   * The output datasets by default will then          \n"
    "                     be in 1D format themselves.  To have them         \n"
    "                     formatted as AFNI datasets instead, use           \n"
    "                       -DAFNI_WRITE_1D_AS_PREFIX=YES                   \n"
    "                     on the command line.                              \n"
    "                   * You should use '-force_TR' to set the TR of       \n"
    "                     the 1D 'dataset' if you use '-input' rather       \n"
    "                     than '-input1D' [the default is 1.0 sec].         \n"
    "\n"
    "-sat OR -trans     * 3dDeconvolve can check the dataset time series    \n"
    "                     for initial saturation transients, which should   \n"
    "                     normally have been excised before data analysis.  \n"
    "                     If you want to have it do this somewhat time      \n"
    "                     consuming check, use the option '-sat'.           \n"
    "                   * Or set environment variable AFNI_SKIP_SATCHECK to NO.\n"
    "                   * Program 3dSatCheck does this check, also.         \n"
    "\n"
    "[-noblock]           Normally, if you input multiple datasets with     \n"
    "                     '-input', then the separate datasets are taken to \n"
    "                     be separate image runs that get separate baseline \n"
    "                     models.  If you want to have the program consider \n"
    "                     these to be all one big run, use -noblock.        \n"
    "                   * If any of the input dataset has only 1 sub-brick, \n"
    "                     then this option is automatically invoked!        \n"
    "                   * If the auto-catenation feature isn't used, then   \n"
    "                     this option has no effect, no how, no way.        \n"
    "\n"
    "[-force_TR TR]       Use this value of TR instead of the one in        \n"
    "                     the -input dataset.                               \n"
    "                     (It's better to fix the input using 3drefit.)     \n"
    "\n"
    "[-input1D dname]     dname = filename of single (fMRI) .1D time series \n"
    "                             where time run downs the column.          \n"
    "\n"
    "[-TR_1D tr1d]        tr1d = TR for .1D time series [default 1.0 sec].  \n"
    "                     This option has no effect without -input1D        \n"
    "\n"
    "[-nodata [NT [TR]]   Evaluate experimental design only (no input data) \n"
    "                   * Optional, but highly recommended: follow the      \n"
    "                     '-nodata' with two numbers, NT=number of time     \n"
    "                     points, and TR=time spacing between points (sec)  \n"
    "\n"
    "[-mask mname]        mname = filename of 3D mask dataset               \n"
    "                      Only data time series from within the mask       \n"
    "                      will be analyzed; results for voxels outside     \n"
    "                      the mask will be set to zero.                    \n"
    "\n"
    "[-automask]          Build a mask automatically from input data        \n"
    "                      (will be slow for long time series datasets)     \n"
    "                  ** If you don't specify ANY mask, the program will   \n"
    "                      build one automatically (from each voxel's RMS)  \n"
    "                      and use this mask solely for the purpose of      \n"
    "                      reporting truncation-to-short errors (if '-short'\n"
    "                      is used) AND for computing the FDR curves in the \n"
    "                      bucket dataset's header (unless '-noFDR' is used,\n"
    "                      of course).                                      \n"
    "                   * If you don't want the FDR curves to be computed   \n"
    "                      inside this automatically generated mask, then   \n"
    "                      use '-noFDR' and later run '3drefit -addFDR' on  \n"
    "                      the bucket dataset.                              \n"
    "                   * To be precise, the above default masking only     \n"
    "                      happens when you use '-input' to run the program \n"
    "                      with a 3D+time dataset; not with '-input1D'.     \n"
    "\n"
    "[-STATmask sname]    Build a mask from file 'sname', and use this      \n"
    "                       mask for the purpose of reporting truncation-to \n"
    "                       float issues AND for computing the FDR curves.  \n"
    "                       The actual results ARE not masked with this     \n"
    "                       option (only with '-mask' or '-automask' options)\n"
    "                       * If you don't use '-STATmask', then the mask   \n"
    "                         from '-mask' or '-automask' is used for these \n"
    "                         purposes.  If neither of those is given, then \n"
    "                         the automatically generated mask described    \n"
    "                         just above is used for these purposes.        \n"
    "\n"
    "[-censor cname]      cname = filename of censor .1D time series        \n"
    "                   * This is a file of 1s and 0s, indicating which     \n"
    "                     time points are to be included (1) and which are  \n"
    "                     to be excluded (0).                               \n"
    "                   * Option '-censor' can only be used once!           \n"
    "                   * The option below may be simpler to use!           \n"
    "\n"
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
    "\n"
    "[-concat rname]      rname = filename for list of concatenated runs    \n"
    "                      * 'rname' can be in the format                   \n"
    "                          '1D: 0 100 200 300'                          \n"
    "                        which indicates 4 runs, the first of which     \n"
    "                        starts at time index=0, second at index=100,   \n"
    "                        and so on.                                     \n"
    "\n"
    "[-nfirst fnum]       fnum = number of first dataset image to use in the\n"
    "                       deconvolution procedure. [default = max maxlag] \n"
    "\n"
    "[-nlast  lnum]       lnum = number of last dataset image to use in the \n"
    "                       deconvolution procedure. [default = last point] \n"
    "\n"
    "[-polort pnum]       pnum = degree of polynomial corresponding to the  \n"
    "                       null hypothesis  [default: pnum = 1]            \n"
    "                    ** For pnum > 2, this type of baseline detrending  \n"
    "                       is roughly equivalent to a highpass filter      \n"
    "                       with a cutoff of (p-2)/D Hz, where 'D' is the   \n"
    "                       duration of the imaging run: D = N*TR           \n"
    "                    ** If you use 'A' for pnum, the program will       \n"
    "                       automatically choose a value based on the       \n"
    "                       time duration D of the longest run:             \n"
    "                         pnum = 1 + int(D/150)                         \n"
    "                ==>>** 3dDeconvolve is the ONLY AFNI program with the  \n"
    "                       -polort option that allows the use of 'A' to    \n"
    "                       set the polynomial order automatically!!!       \n"
    "                    ** Use '-1' for pnum to specifically NOT include   \n"
    "                       any polynomials in the baseline model.  Only    \n"
    "                       do this if you know what this means!            \n"
    "\n"
    "[-legendre]          use Legendre polynomials for null hypothesis      \n"
    "                       (baseline model)                                \n"
    "\n"
    "[-nolegendre]        use power polynomials for null hypotheses         \n"
    "                       [default is -legendre]                          \n"
    "                    ** Don't do this unless you are crazy!             \n"
    "\n"
    "[-nodmbase]          don't de-mean baseline time series                \n"
    "                       (i.e., polort>0 and -stim_base inputs)          \n"
    "[-dmbase]            de-mean baseline time series [default if polort>=0]\n"
    "\n"
    "[-svd]               Use SVD instead of Gaussian elimination [default] \n"
    "[-nosvd]             Use Gaussian elimination instead of SVD           \n"
    "                       (only use for testing + backwards compatibility)\n"
    "\n"
    "[-rmsmin r]          r = minimum rms error to reject reduced model     \n"
    "                       (default = 0; don't use this option normally!)  \n"
    "\n"
    "[-nocond]            DON'T calculate matrix condition number           \n"
    "                      ** This value is NOT the same as Matlab!         \n"
    "\n"
    "[-singvals]          Print out the matrix singular values              \n"
    "                      (useful for some testing/debugging purposes)     \n"
    "                      Also see program 1dsvd.                          \n"
    "\n"
    "[-GOFORIT [g]]       Use this to proceed even if the matrix has        \n"
    "                     bad problems (e.g., duplicate columns, large      \n"
    "                     condition number, etc.).                          \n"
    "               *N.B.: Warnings that you should particularly heed have  \n"
    "                      the string '!!' somewhere in their text.         \n"
    "               *N.B.: Error and Warning messages go to stderr and      \n"
    "                      also to file " PROGRAM_NAME ".err.               \n"
    "                      ++ You can disable the creation of this .err     \n"
    "                         file by setting environment variable          \n"
    "                         AFNI_USE_ERROR_FILE to NO before running      \n"
    "                         this program.                                 \n"
    "               *N.B.: The optional number 'g' that appears is the      \n"
    "                      number of warnings that can be ignored.          \n"
    "                      That is, if you use -GOFORIT 7 and 9 '!!'        \n"
    "                      matrix warnings appear, then the program will    \n"
    "                      not run.  If 'g' is not present, 1 is used.      \n"
    "\n"
    "[-allzero_OK]        Don't consider all zero matrix columns to be      \n"
    "                      the type of error that -GOFORIT is needed to     \n"
    "                      ignore.                                          \n"
    "                     * Please know what you are doing when you use     \n"
    "                       this option!                                    \n"
    "\n"
    "[-Dname=val]       = Set environment variable 'name' to 'val' for this \n"
    "                     run of the program only.                          \n"
    "                                                                       \n"
    "******* Input stimulus options *******                                 \n"
    "                                                                       \n"
    "-num_stimts num      num = number of input stimulus time series        \n"
    "                       (0 <= num)   [default: num = 0]                 \n"
    "               *N.B.: '-num_stimts' must come before any of the        \n"
    "                      following '-stim' options!                       \n"
    "               *N.B.: Most '-stim' options have as their first argument\n"
    "                      an integer 'k', ranging from 1..num, indicating  \n"
    "                      which stimulus class the argument is defining.   \n"
    "               *N.B.: The purpose of requiring this option is to make  \n"
    "                      sure your model is complete -- that is, you say  \n"
    "                      you are giving 5 '-stim' options, and then the   \n"
    "                      program makes sure that all of them are given    \n"
    "                      -- that is, that you don't forget something.     \n"
    "                                                                       \n"
    "-stim_file k sname   sname = filename of kth time series input stimulus\n"
    "               *N.B.: This option directly inserts a column into the   \n"
    "                      regression matrix; unless you are using the 'old'\n"
    "                      method of deconvolution (cf below), you would    \n"
    "                      normally only use '-stim_file' to insert baseline\n"
    "                      model components such as motion parameters.      \n"
    "                                                                       \n"
    "[-stim_label k slabel] slabel = label for kth input stimulus           \n"
    "               *N.B.: This option is highly recommended, so that       \n"
    "                      output sub-bricks will be labeled for ease of    \n"
    "                      recognition when you view them in the AFNI GUI.  \n"
    "                                                                       \n"
    "[-stim_base k]       kth input stimulus is part of the baseline model  \n"
    "               *N.B.: 'Baseline model' == Null Hypothesis model        \n"
    "               *N.B.: The most common baseline components to add are   \n"
    "                      the 6 estimated motion parameters from 3dvolreg. \n"
    "\n"
    "-ortvec fff lll      This option lets you input a rectangular array    \n"
    "                     of 1 or more baseline vectors from file 'fff',    \n"
    "                     which will get the label 'lll'.  Functionally,    \n"
    "                     it is the same as using '-stim_file' on each      \n"
    "                     column of 'fff' separately (plus '-stim_base').   \n"
    "                     This method is just a faster and simpler way to   \n"
    "                     include a lot of baseline regressors in one step. \n"
    "          -->>**N.B.: This file is NOT included in the '-num_stimts'   \n"
    "                      count that you provide.                          \n"
    "               *N.B.: These regression matrix columns appear LAST      \n"
    "                      in the matrix, after everything else.            \n"
    "               *N.B.: You can use column '[..]' and/or row '{..}'      \n"
    "                      selectors on the filename 'fff' to pick out      \n"
    "                      a subset of the numbers in that file.            \n"
    "               *N.B.: The q-th column of 'fff' will get a label        \n"
    "                      like 'lll[q]' in the 3dDeconvolve results.       \n"
    "               *N.B.: This option is known as the 'Inati Option'.      \n"
    "               *N.B.: Unlike the original 'Inati' (who is unique), it  \n"
    "                      is allowed to have more than one '-ortvec' option.\n"
    "               *N.B.: Program 1dBport is one place to generate a file  \n"
    "                      for use with '-ortvec'; 1deval might be another. \n"
    "\n"
    "**N.B.: You must have -num_stimts > 0  AND/OR                          \n"
    "        You must use  -ortvec          AND/OR                          \n"
    "        You must have -polort >= 0                                     \n"
    "        Otherwise, there is no regression model!                       \n"
    "        An example using -polort only:                                 \n"
    " 3dDeconvolve -x1D_stop -polort A -nodata 300 2 -x1D stdout: | 1dplot -one -stdin\n"
    "\n"
    "**N.B.: The following 3 options are for the 'old' style of explicit    \n"
    "        deconvolution.  For most purposes, their usage is no longer    \n"
    "        recommended.  Instead, you should use the '-stim_times' options\n"
    "        to directly input the stimulus times, rather than code the     \n"
    "        stimuli as a sequence of 0s and 1s in this 'old' method!       \n"
    "\n"
    "[-stim_minlag k m]   m = minimum time lag for kth input stimulus       \n"
    "                       [default: m = 0]                                \n"
    "[-stim_maxlag k n]   n = maximum time lag for kth input stimulus       \n"
    "                       [default: n = 0]                                \n"
    "[-stim_nptr k p]     p = number of stimulus function points per TR     \n"
    "                       Note: This option requires 0 slice offset times \n"
    "                       [default: p = 1]                                \n"
    "                                                                       \n"
    "**N.B.: The '-stim_times' options below are the recommended way of     \n"
    "        analyzing FMRI time series data now.  The options directly     \n"
    "        above are only maintained for the sake of backwards            \n"
    "        compatibility!  For most FMRI users, the 'BLOCK' and 'TENT'    \n"
    "        (or 'CSPLIN') response models will serve their needs.  The     \n"
    "        other models are for users with specific needs who understand  \n"
    "        clearly what they are doing.                                   \n"
    "                                                                       \n"
    "[-stim_times k tname Rmodel]                                           \n"
    "   Generate the k-th response model from a set of stimulus times       \n"
    "   given in file 'tname'.                                              \n"
    "    *** The format of file 'tname' is one line per imaging run         \n"
    "        (cf. '-concat' above), and each line contains the list of START\n"
    "        times (in seconds) for the stimuli in class 'k' for its        \n"
    "        corresponding run of data; times are relative to the start of  \n"
    "        the run (i.e., sub-brick #0 occurring at time=0).              \n"
    "    *** The DURATION of the stimulus is encoded in the 'Rmodel'        \n"
    "        argument, described below. Units are in seconds, not TRs!      \n"
    "        -- If different stimuli in the same class 'k' have different   \n"
    "           durations, you'll have to use the dmBLOCK response model    \n"
    "           and '-stim_times_AM1' or '-stim_times_AM2', described below.\n"
    "    *** Different lines in the 'tname' file can contain different      \n"
    "        numbers of start times.  Each line must contain at least 1 time.\n"
    "    *** If there is no stimulus in class 'k' in a particular imaging   \n"
    "        run, there are two ways to indicate that:                      \n"
    "          (a) put a single '*' on the line, or                         \n"
    "          (b) put a very large number or a negative number             \n"
    "              (e.g., 99999, or -1) on the line                         \n"
    "              -- times outside the range of the imaging run will cause \n"
    "                 a warning message, but the program will soldier on.   \n"
    "    *** In the case where the stimulus doesn't actually exist in the   \n"
    "        data model (e.g., every line in 'tname' is a '*'), you will    \n"
    "        also have to use the '-allzero_OK' option to force 3dDeconvolve\n"
    "        to run with regressor matrix columns that are filled with zeros.\n"
    "                                                                       \n"
    "   The response model is specified by the third argument after         \n"
    "   '-stim_times' ('Rmodel'), and can be one of the following:          \n"
    "    *** In the descriptions below, a '1 parameter' model has a fixed   \n"
    "        shape, and only the estimated amplitude ('Coef') varies:       \n"
    "          BLOCK GAM TWOGAM SPMG1 WAV MION                              \n"
    "    *** Models with more than 1 parameter have multiple basis          \n"
    "        functions, and the estimated parameters ('Coef') are their     \n"
    "        amplitudes. The estimated shape of the response to a stimulus  \n"
    "        will be different in different voxels:                         \n"
    "          TENT CSPLIN SPMG2 SPMG3 POLY SIN EXPR                        \n"
    "    *** Many models require the input of the start and stop times for  \n"
    "        the response, 'b' and 'c'.  Normally, 'b' would be zero, but   \n"
    "        in some cases, 'b' could be negative -- for example, if you    \n"
    "        are concerned about anticipatory effects.  The stop time 'c'   \n"
    "        should be based on how long you realistically expect the       \n"
    "        hemodynamic response to last after the onset of the stimulus;  \n"
    "        e.g., the duration of the stimulus plus 14 seconds for BOLD.   \n"
    "    *** If you use '-tout', each parameter will get a separate         \n"
    "        t-statistic.  As mentioned far above, this is a marginal       \n"
    "        statistic, measuring the impact of that model component on the \n"
    "        regression fit, relative to the fit with that one component    \n"
    "        (matrix column) removed.                                       \n"
    "    *** If you use '-fout', each stimulus will also get an F-statistic,\n"
    "        which is the collective impact of all the model components     \n"
    "        it contains, relative to the regression fit with the entire    \n"
    "        stimulus removed. (If there is only 1 parameter, then F = t*t.)\n"
    "    *** Some models below are described in terms of a simple response  \n"
    "        function that is then convolved with a square wave whose       \n"
    "        duration is a parameter you give (duration is NOT a parameter  \n"
    "        that will be estimated).  Read the descriptions below carefully:\n"
    "        not all functions are (or can be) convolved in this way:       \n"
    "         * ALWAYS convolved:      BLOCK  dmBLOCK  MION  MIONN          \n"
    "         * OPTIONALLY convolved:  GAM    TWOGAM   SPMGx WAV            \n"
    "         * NEVER convolved:       TENT   CSPLIN   POLY  SIN   EXPR     \n"
    "        Convolution is specified by providing the duration parameter   \n"
    "        as described below for each particular model function.         \n"
    "\n"
    "     'BLOCK(d,p)'  = 1 parameter block stimulus of duration 'd'        \n"
    "                    ** There are 2 variants of BLOCK:                  \n"
    "                         BLOCK4 [the default] and BLOCK5               \n"
    "                       which have slightly different delays:           \n"
    "                         HRF(t) = int( g(t-s) , s=0..min(t,d) )        \n"
    "                       where g(t) = t^q * exp(-t) /(q^q*exp(-q))       \n"
    "                       and q = 4 or 5.  The case q=5 is delayed by     \n"
    "                       about 1 second from the case q=4.               \n"
    "                ==> ** Despite the name, you can use 'BLOCK' for event-\n"
    "                       related analyses just by setting the duration to\n"
    "                       a small value; e.g., 'BLOCK5(1,1)'              \n"
    "                    ** The 'p' parameter is the amplitude of the       \n"
    "                       basis function, and should usually be set to 1. \n"
    "                       If 'p' is omitted, the amplitude will depend on \n"
    "                       the duration 'd', which is useful only in       \n"
    "                       special circumstances!!                         \n"
    "                    ** For bad historical reasons, the peak amplitude  \n"
    "                       'BLOCK' without the 'p' parameter does not go to\n"
    "                       1 as the duration 'd' gets large.  Correcting   \n"
    "                       this oversight would break some people's lives, \n"
    "                       so that's just the way it is.                   \n"
    "                    ** The 'UBLOCK' function (U for Unit) is the same  \n"
    "                       as the 'BLOCK' function except that when the    \n"
    "                       'p' parameter is missing (or 0), the peak       \n"
    "                       amplitude goes to 1 as the duration gets large. \n"
    "                       If p > 0, 'UBLOCK(d,p)' and 'BLOCK(d,p)' are    \n"
    "                       identical.                                      \n"
    "\n"
    "     'TENT(b,c,n)' = n parameter tent function expansion from times    \n"
    "                       b..c after stimulus time [piecewise linear]     \n"
    "                       [n must be at least 2; time step is (c-b)/(n-1)]\n"
    "    'CSPLIN(b,c,n)'= n parameter cubic spline function expansion       \n"
    "                       from times b..c after stimulus time             \n"
    "                       [n must be at least 4]                          \n"
    "                     ** CSPLIN is a drop-in upgrade of TENT to a       \n"
    "                        differentiable set of functions.               \n"
    "                     ** TENT and CSPLIN are 'cardinal' interpolation   \n"
    "                        functions: their parameters are the values     \n"
    "                        of the HRF model at the n 'knot' points        \n"
    "                          b , b+dt , b+2*dt , ... [dt = (c-b)/(n-1)]   \n"
    "                        In contrast, in a model such as POLY or SIN,   \n"
    "                        the parameters output are not directly the     \n"
    "                        hemodynamic response function values at any    \n"
    "                        particular point.                              \n"
    "                 ==> ** You can also use 'TENTzero' and 'CSPLINzero',  \n"
    "                        which means to eliminate the first and last    \n"
    "                        basis functions from each set.  The effect     \n"
    "                        of these omissions is to force the deconvolved \n"
    "                        HRF to be zero at t=b and t=c (to start and    \n"
    "                        and end at zero response).  With these 'zero'  \n"
    "                        response models, there are n-2 parameters      \n"
    "                        (thus for 'TENTzero', n must be at least 3).   \n"
    "                     ** These 'zero' functions will force the HRF to   \n"
    "                        be continuous, since they will now be unable   \n"
    "                        to suddenly rise up from 0 at t=b and/or drop  \n"
    "                        down to 0 at t=c.                              \n"
    "\n"
    "     'GAM(p,q)'    = 1 parameter gamma variate                         \n"
    "                         (t/(p*q))^p * exp(p-t/q)                      \n"
    "                       Defaults: p=8.6 q=0.547 if only 'GAM' is used   \n"
    "                     ** The peak of 'GAM(p,q)' is at time p*q after    \n"
    "                        the stimulus.  The FWHM is about 2.35*sqrt(p)*q;\n"
    "                        this approximation is accurate for p > 0.3*q.  \n"
    "                     ** To check this approximation, try the command   \n"
    "               1deval -num 100 -del 0.02 -xzero 0.02   \\\n"
    "                      -expr 'sqrt(gamp(x,1))/2.35/x' | \\\n"
    "               1dplot -stdin -del 0.02 -xzero 0.02 -yaxis 1:1.4:4:10   \n"
    "                        If the two functions gamp(x,1) and 2.35*x      \n"
    "                        were equal, the plot would be constant y=1.    \n"
    "                 ==> ** If you add a third argument 'd', then the GAM  \n"
    "                        function is convolved with a square wave of    \n"
    "                        duration 'd' seconds; for example:             \n"
    "                          'GAM(8.6,.547,17)'                           \n"
    "                        for a 17 second stimulus.  [09 Aug 2010]       \n"
    "     'GAMpw(K,W)'  = Same as 'GAM(p,q)' but where the shape parameters \n"
    "                       are specified at time to peak 'K' and full      \n"
    "                       width at half max (FWHM) 'W'. You can also      \n"
    "                       add a third argument as the duration. The (K,W) \n"
    "                       parameters are converted to (p,q) values for    \n"
    "                       the actual computations; the (p,q) parameters   \n"
    "                       are printed to the text (stderr) output.        \n"
    "                     ** Note that if you give weird values for K and W,\n"
    "                        weird things will happen: (tcsh syntax)        \n"
    "                         set pp = `ccalc 'gamp(2,8)'`                  \n"
    "                         set qq = `ccalc 'gamq(2,8)'`                  \n"
    "                         1deval -p=$pp -q=$qq -num 200 -del 0.1  \\\n"
    "                                -expr '(t/p/q)^p*exp(p-t/q)'   | \\\n"
    "                                1dplot -stdin -del 0.1                 \n"
    "                        Here, K is significantly smaller than W,       \n"
    "                        so a gamma variate that fits peak=2 width=8    \n"
    "                        must be weirdly shaped. [Also note use of the  \n"
    "                        'calc' functions gamp(K,W) and gamq(K,W) to    \n"
    "                        calculate p and q from K and W in the script.] \n"
    "\n"
    "     'TWOGAM(p1,q1,r,p2,q2)'                                           \n"
    "                   = 1 parameter (amplitude) model:                    \n"
    "                   = A combination of two 'GAM' functions:             \n"
    "                         GAM(p1,q1) - r*GAM(p2,q2)                     \n"
    "                       This model is intended to let you use a HRF     \n"
    "                       similar to BrainVoyager (e.g.). You can         \n"
    "                       add a sixth argument as the duration.           \n"
    "                     ** Note that a positive 'r' parameter means to    \n"
    "                        subtract the second GAM function (undershoot). \n"
    "     'TWOGAMpw(K1,W1,r,K2,W2)'                                         \n"
    "                   = Same as above, but where the peaks and widths     \n"
    "                       of the 2 component gamma variates are given     \n"
    "                       instead of the less intuitive p and q.          \n"
    "                       For FMRI work, K2 > K1 is usual, as the         \n"
    "                       second (subtracted) function is intended        \n"
    "                       to model the 'undershoot' after the main        \n"
    "                       positive part of the model. You can also        \n"
    "                       add a sixth argument as the duration.           \n"
    "                     ** Example (no duration given):                   \n"
    "        3dDeconvolve -num_stimts 1 -polort -1 -nodata 81 0.5         \\\n"
    "                     -stim_times 1 '1D: 0' 'TWOGAMpw(3,6,0.2,10,12)' \\\n"
    "                     -x1D stdout: | 1dplot -stdin -THICK -del 0.5      \n"
    "\n"
    "     'SPMG1'       = 1 parameter SPM gamma variate basis function      \n"
    "                         exp(-t)*(A1*t^P1-A2*t^P2) where               \n"
    "                       A1 = 0.0083333333  P1 = 5  (main positive lobe) \n"
    "                       A2 = 1.274527e-13  P2 = 15 (undershoot part)    \n"
    "                       This function is NOT normalized to have peak=1! \n"
    "     'SPMG2'       = 2 parameter SPM: gamma variate + d/dt derivative  \n"
    "                       [For backward compatibility: 'SPMG' == 'SPMG2'] \n"
    "     'SPMG3'       = 3 parameter SPM basis function set                \n"
    "                 ==> ** The SPMGx functions now can take an optional   \n"
    "                        (duration) argument, specifying that the primal\n"
    "                        SPM basis functions should be convolved with   \n"
    "                        a square wave 'duration' seconds long and then \n"
    "                        be normalized to have peak absolute value = 1; \n"
    "                        e.g., 'SPMG3(20)' for a 20 second duration with\n"
    "                        three basis function.  [28 Apr 2009]           \n"
    "                     ** Note that 'SPMG1(0)' will produce the usual    \n"
    "                        'SPMG1' wavefunction shape, but normalized to  \n"
    "                        have peak value = 1 (for example).             \n"
    "\n"
    "     'POLY(b,c,n)' = n parameter Legendre polynomial expansion         \n"
    "                       from times b..c after stimulus time             \n"
    "                       [n can range from 1 (constant) to 20]           \n"
    "\n"
    "     'SIN(b,c,n)'  = n parameter sine series expansion                 \n"
    "                       from times b..c after stimulus time             \n"
    "                       [n must be at least 1]                          \n"
    "\n"
    "     'WAV(d)'      = 1 parameter block stimulus of duration 'd'.       \n"
    "                      * This is the '-WAV' function from program waver!\n"
    "                      * If you wish to set the shape parameters of the \n"
    "                        WAV function, you can do that by adding extra  \n"
    "                        arguments, in the order                        \n"
    "                         delay time , rise time , fall time ,          \n"
    "                         undershoot fraction, undershoot restore time  \n"
    "                      * The default values are 'WAV(d,2,4,6,0.2,2)'    \n"
    "                      * Omitted parameters get the default values.     \n"
    "                      * 'WAV(d,,,,0)' (setting undershoot=0) is        \n"
    "                        very similar to 'BLOCK5(d,1)', for d > 0.      \n"
    "                      * Setting duration d to 0 (or just using 'WAV')  \n"
    "                        gives the pure '-WAV' impulse response function\n"
    "                        from waver.                                    \n"
    "                      * If d > 0, the WAV(0) function is convolved with\n"
    "                        a square wave of duration d to make the HRF,   \n"
    "                        and the amplitude is scaled back down to 1.    \n"
    "\n"
    "     'EXPR(b,c) exp1 ... expn'                                         \n"
    "                   = n parameter; arbitrary expressions from times     \n"
    "                     b..c after stimulus time                          \n"
    "                      * Expressions are separated by spaces, so        \n"
    "                        each expression must be a contiguous block     \n"
    "                        of non-whitespace characters                   \n"
    "                      * The entire model, from 'EXPR' to the final     \n"
    "                        expression must be enclosed in one set of      \n"
    "                        quotes. The individual component expressions   \n"
    "                        are separated by blanks. Example:              \n"
    "                          '-EXPR(0,20) sin(PI*t/20)^2'                 \n"
    "                      * Expressions use the same format as 3dcalc      \n"
    "                      * Symbols that can be used in an expression:     \n"
    "                         t = time in sec since stimulus time           \n"
    "                         x = time scaled to be x= 0..1 for t=bot..top  \n"
    "                         z = time scaled to be z=-1..1 for t=bot..top  \n"
    "                      * Spatially dependent regressors are not allowed!\n"
    "                      * Other symbols are set to 0 (silently).         \n"
    "                 ==> ** There is no convolution of the 'EXPR' functions\n"
    "                        with a square wave implied.  The expressions   \n"
    "                        you input are what you get, evaluated over     \n"
    "                        times b..c after each stimulus time.  To be    \n"
    "                        sure of what your response model is, you should\n"
    "                        plot the relevant columns from the matrix      \n"
    "                        .xmat.1D output file.                          \n"
    "\n"
    "     'MION(d)'     = 1 parameter block stimulus of duration 'd',       \n"
    "                     intended to model the response of MION.           \n"
    "                     The zero-duration impulse response 'MION(0)' is   \n"
    "                       h(t) = 16.4486 * ( -0.184/ 1.5 * exp(-t/ 1.5)   \n"
    "                                          +0.330/ 4.5 * exp(-t/ 4.5)   \n"
    "                                          +0.670/13.5 * exp(-t/13.5) ) \n"
    "                     which is adapted from the paper                   \n"
    "                      FP Leite, et al. NeuroImage 16:283-294 (2002)    \n"
    "                      http://dx.doi.org/10.1006/nimg.2002.1110         \n"
    "                  ** Note that this is a positive function, but MION   \n"
    "                     produces a negative response to activation, so the\n"
    "                     beta and t-statistic for MION are usually negative.\n"
    "               ***** If you want a negative MION function (so you get  \n"
    "                     a positive beta), use the name 'MIONN' instead.   \n"
    "                  ** After convolution with a square wave 'd' seconds  \n"
    "                     long, the resulting single-trial waveform is      \n"
    "                     scaled to have magnitude 1.  For example, try     \n"
    "                     this fun command to compare BLOCK and MION:       \n"
    "               3dDeconvolve -nodata 300 1 -polort -1 -num_stimts 2   \\\n"
    "                            -stim_times 1 '1D: 10 150' 'MION(70)'    \\\n"
    "                            -stim_times 2 '1D: 10 150' 'BLOCK(70,1)' \\\n"
    "                            -x1D stdout: | 1dplot -stdin -one -thick   \n"
    "                     You will see that the MION curve rises and falls  \n"
    "                     much more slowly than the BLOCK curve.            \n"
    "              ==> ** Note that 'MION(d)' is already convolved with a   \n"
    "                     square wave of duration 'd' seconds.  Do not      \n"
    "                     convolve it again by putting in multiple closely  \n"
    "                     spaced stimulus times (this mistake has been made)!\n"
    "                  ** Scaling the single-trial waveform to have magnitude\n"
    "                     1 means that trials with different durations 'd'  \n"
    "                     will have the same magnitude for their regression \n"
    "                     models.                                           \n"
    "                                                                       \n"
    " * 3dDeconvolve does LINEAR regression, so the model parameters are    \n"
    "   amplitudes of the basis functions; 1 parameter models are 'simple'  \n"
    "   regression, where the shape of the impulse response function is     \n"
    "   fixed and only the magnitude/amplitude varies.  Models with more    \n"
    "   free parameters have 'variable' shape impulse response functions.   \n"
    "\n"
    " * LINEAR regression means that each data time series (thought of as   \n"
    "   a single column of numbers = a vector) is fitted to a sum of the    \n"
    "   matrix columns, each one multiplied by an amplitude parameter to    \n"
    "   be calculated ('Coef'). The purpose of the various options          \n"
    "     '-stim_times', '-polort', '-ortvec', and/or '-stim_file'          \n"
    "   is to build the columns of the regression matrix.                   \n"
    "                                                                       \n"
    " * If you want NONLINEAR regression, see program 3dNLfim.              \n"
    "                                                                       \n"
    " * If you want LINEAR regression with allowance for non-white noise,   \n"
    "   use program 3dREMLfit, after using 3dDeconvolve to set up the       \n"
    "   regression model (in the form of a matrix file).                    \n"
    "                                                                       \n"
    "** When in any doubt about the shape of the response model you are   **\n"
    "*  asking for, you should plot the relevant columns from the X matrix *\n"
    "*  to help develop some understanding of the analysis.  The 'MION'    *\n"
    "*  example above can be used as a starting point for how to easily    *\n"
    "*  setup a quick command pipeline to graph response models.  In that  *\n"
    "*  example, '-polort -1' is used to suppress the usual baseline model *\n"
    "*  since graphing that part of the matrix would just be confusing.    *\n"
    "*  Another example, for example, comparing the similar models         *\n"
    "** 'WAV(10)', 'BLOCK4(10,1)', and 'SPMG1(10)':                       **\n"
    "                                                                       \n"
    "     3dDeconvolve -nodata 100 1.0 -num_stimts 3 -polort -1   \\\n"
    "                  -local_times -x1D stdout:                  \\\n"
    "                  -stim_times 1 '1D: 10 60' 'WAV(10)'        \\\n"
    "                  -stim_times 2 '1D: 10 60' 'BLOCK4(10,1)'   \\\n"
    "                  -stim_times 3 '1D: 10 60' 'SPMG1(10)'      \\\n"
    "      | 1dplot -thick -one -stdin -xlabel Time -ynames WAV BLOCK4 SPMG1\n"
    "                                                                       \n"
    " * For the format of the 'tname' file, see the last part of            \n"
    " https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/DeconSummer2004.html \n"
    "   and also see the other documents stored in the directory below:     \n"
    " https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/                     \n"
    "   and also read the presentation below:                               \n"
    " https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni05_regression.pdf\n"
    "  ** Note Well:                                                        \n"
    "   * The contents of the 'tname' file are NOT just 0s and 1s,          \n"
    "     but are the actual times of the stimulus events IN SECONDS.       \n"
    "   * You can give the times on the command line by using a string      \n"
    "     of the form '1D: 3.2 7.9 | 8.2 16.2 23.7' in place of 'tname',    \n"
    "     where the '|' character indicates the start of a new line         \n"
    "     (so this example is for a case with 2 catenated runs).            \n"
    "=> * You CANNOT USE the '1D:' form of input for any of the more        \n"
    "     complicated '-stim_times_*' options below!!                       \n"
    "   * The '1D:' form of input is mostly useful for quick tests, as      \n"
    "     in the examples above, rather than for production analyses with   \n"
    "     lots of different stimulus times and multiple imaging runs.       \n"
    "                                                                       \n"
    "[-stim_times_AM1 k tname Rmodel]                                       \n"
    "   Similar, but generates an amplitude modulated response model.       \n"
    "   The 'tname' file should consist of 'time*amplitude' pairs.          \n"
    "   As in '-stim_times', the '*' character can be used as a placeholder \n"
    "   when an imaging run doesn't have any stimulus of a given class.     \n"
    "   *N.B.: What I call 'amplitude' modulation is called 'parametric'    \n"
    "          modulation in Some other PrograM.                            \n"
    " ***N.B.: If NO run at all has a stimulus of a given class, then you   \n"
    "          must have at least 1 time that is not '*' for -stim_times_*  \n"
    "          to work (so that the proper number of regressors can be set  \n"
    "          up).  You can use a negative time for this purpose, which    \n"
    "          will produce a warning message but otherwise will be         \n"
    "          ignored, as in:                                              \n"
    "             -1*37                                                     \n"
    "             *                                                         \n"
    "          for a 2 run 'tname' file to be used with -stim_times_*.      \n"
    "       ** In such a case, you will also need the -allzero_OK option,   \n"
    "          and probably -GOFORIT as well.                               \n"
    "\n"
    "[-stim_times_AM2 k tname Rmodel]                                       \n"
    "   Similar, but generates 2 response models: one with the mean         \n"
    "   amplitude and one with the differences from the mean.               \n"
    "  *** Please note that 'AM2' is the option you should probably use!    \n"
    "  *** 'AM1' is for special cases, and normally should not be used      \n"
    "      for FMRI task activation analyses!!                              \n"
    "  *** 'AM2' will give you the ability to detect voxels that activate   \n"
    "      but do not change proportional to the amplitude factor, as well  \n"
    "      as provide a direct measure of the proportionality of the        \n"
    "      activation to changes in the input amplitude factors.  'AM1'     \n"
    "      will do neither of these things.                                 \n"
    "  *** Normally, 3dDeconvolve removes the mean of the auxiliary         \n"
    "      parameter(s) from the modulated regressor(s).  However, if you   \n"
    "      set environment variable AFNI_3dDeconvolve_rawAM2 to YES, then   \n"
    "      the mean will NOT be removed from the auxiliary parameter(s).    \n"
    "      This ability is provided for users who want to center their      \n"
    "      parameters using their own method.                               \n"
    "  *** [12 Jul 2012] You can now specify the value to subtract from     \n"
    "      each modulation parameter -- this value will replace the         \n"
    "      subtraction of the average parameter value that usually happens. \n"
    "      To do this, add an extra parameter after the option, as in       \n"
    "        -stim_times_AM2 1 timesAM.1D 'BLOCK(2,1)' :5.2:x:2.0           \n"
    "      The extra argument must start with the colon ':' character, and  \n"
    "      there should be as many different values (separated by ':') as   \n"
    "      there are parameters in the timing file (timesAM.1D above).      \n"
    "  ==> In the example above, ':5.2:x:2.0' means                         \n"
    "        subtract 5.2 from each value of the first parameter in timesAM.1D\n"
    "        subtract the MEAN from each value of the second parameter      \n"
    "          (since 'x' doesn't translate to a number)                    \n"
    "        subtract 2.0 from each value of the third parameter            \n"
    "  ==> What is this option for, anyway?  The purpose is to facilitate   \n"
    "      GROUP analysis the results from a collection of subjects, where  \n"
    "      you want to treat each subject's analysis exactly the same       \n"
    "      way -- and thus, the subtraction value for a parameter (e.g.,    \n"
    "      reaction time) should then be the mean over all the reaction     \n"
    "      times from all trials in all subjects.                           \n"
    "                                                                       \n"
    "** NOTE [04 Dec 2008] **                                               \n"
    " -stim_times_AM1 and -stim_times_AM2 now take files with more          \n"
    "   than 1 amplitude attached to each time; for example,                \n"
    "     33.7*9,-2,3                                                       \n"
    "   indicates a stimulus at time 33.7 seconds with 3 amplitudes         \n"
    "   attached (9 and -2 and 3).  In this example, -stim_times_AM2 would  \n"
    "   generate 4 response models: 1 for the constant response case        \n"
    "   and 1 scaled by each of the amplitude sets.                         \n"
    "   ** Please don't carried away and use too many parameters!! **       \n"
    " For more information on modulated regression, see                     \n"
    "   https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/AMregression.pdf   \n"
    "                                                                       \n"
    "** NOTE [08 Dec 2008] **                                               \n"
    " -stim_times_AM1 and -stim_times_AM2 now have 1 extra response model   \n"
    " function available:                                                   \n"
    "   dmBLOCK (or dmBLOCK4 or dmBLOCK5)                                   \n"
    " where 'dm' means 'duration modulated'.  If you use this response      \n"
    " model, then the LAST married parameter in the timing file will        \n"
    " be used to modulate the duration of the block stimulus.  Any          \n"
    " earlier parameters will be used to modulate the amplitude,            \n"
    " and should be separated from the duration parameter by a ':'          \n"
    " character, as in '30*5,3:12' which means (for dmBLOCK):               \n"
    "   a block starting at 30 s,                                           \n"
    "   with amplitude modulation parameters 5 and 3,                       \n"
    "   and with duration 12 s.                                             \n"
    " The unmodulated peak response of dmBLOCK depends on the duration      \n"
    " of the stimulus, as the BOLD response accumulates.                    \n"
    " If you want the peak response to be a set to a fixed value, use       \n"
    "   dmBLOCK(p)                                                          \n"
    " where p = the desired peak value (e.g., 1).                           \n"
    " *** Understand what you doing when you use dmBLOCK, and look at  ***  \n"
    " *** the regression matrix!  Otherwise, you will end up confused. ***  \n"
    " *N.B.: The maximum allowed dmBLOCK duration is 999 s.                 \n"
    " *N.B.: You cannot use '-iresp' or '-sresp' with dmBLOCK!              \n"
    " *N.B.: If you are NOT doing amplitude modulation at the same time     \n"
    "        (and so you only have 1 'married' parameter per time), use     \n"
    "        '-stim_times_AM1' with dmBLOCK.  If you also want to do        \n"
    "        amplitude modulation at the same time as duration modulation   \n"
    "        (and so you have 2 or more parameters with each time), use     \n"
    "        '-stim_times_AM2' instead.  If you use '-stim_times_AM2' and   \n"
    "        there is only 1 'married' parameter, the program will print    \n"
    "        a warning message, then convert to '-stim_times_AM1', and      \n"
    "        continue -- so nothing bad will happen to your analysis!       \n"
    "        (But you will be embarassed in front of your friends.)         \n"
    " *N.B.: If you are using AM2 (amplitude modulation) with dmBLOCK, you  \n"
    "        might want to use 'dmBLOCK(1)' to make each block have native  \n"
    "        amplitude 1 before it is scaled by the amplitude parameter.    \n"
    "        Or maybe not -- this is a matter for fine judgment.            \n"
    " *N.B.: You can also use dmBLOCK with -stim_times_IM, in which case    \n"
    "        each time in the 'tname' file should have just ONE extra       \n"
    "        parameter -- the duration -- married to it, as in '30:15',     \n"
    "        meaning a block of duration 15 seconds starting at t=30 s.     \n"
    " *N.B.: For bad historical reasons, the peak amplitude dmBLOCK without \n"
    "        the 'p' parameter does not go to 1 as the duration gets large. \n"
    "        Correcting this oversight would break some people's lives, so  \n"
    "        that's just the way it is.                                     \n"
    " *N.B.: The 'dmUBLOCK' function (U for Unit) is the same as the        \n"
    "        'dmBLOCK' function except that when the 'p' parameter is       \n"
    "        missing (or 0), the peak amplitude goes to 1 as the duration   \n"
    "        gets large.  If p > 0, 'dmUBLOCK(p)' and 'dmBLOCK(p)' are      \n"
    "        identical                                                      \n"
    " For some graphs of what dmBLOCK regressors look like, see             \n"
    "   https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/AMregression.pdf   \n"
    " and/or try the following command:                                     \n"
    "    3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 1 \\\n"
    "                 -stim_times_AM1 1 q.1D 'dmBLOCK'       \\\n"
    "                 -x1D stdout: | 1dplot -stdin -thick -thick            \n"
    " where file q.1D contains the single line                              \n"
    "   10:1 40:2 70:3 100:4 130:5 160:6 190:7 220:8 250:9 280:30           \n"
    " Change 'dmBLOCK' to 'dmBLOCK(1)' and see how the matrix plot changes. \n"
    "                                                                       \n"
    " **************** Further notes on dmBLOCK [Nov 2013] **************** \n"
    "                                                                       \n"
    " Basically (IMHO), there are 2 rational choices to use:                \n"
    "                                                                       \n"
    "   (a) 'dmUBLOCK' = allow the amplitude of the response model to       \n"
    "                    vary with the duration of the stimulus; getting    \n"
    "                    larger with larger durations; for durations longer \n"
    "                    than about 15s, the amplitude will become 1.       \n"
    "               -->> This choice is equivalent to 'dmUBLOCK(0)', but    \n"
    "                    is NOT equivalent to 'dmBLOCK(0)' due to the       \n"
    "                    historical scaling issue alluded to above.         \n"
    "                                                                       \n"
    "   (b) 'dmUBLOCK(1)' = all response models will get amplitude 1,       \n"
    "                       no matter what the duration of the stimulus.    \n"
    "                  -->> This choice is equivalent to 'dmBLOCK(1)'.      \n"
    "                                                                       \n"
    " Some users have expressed the desire to allow the amplitude to        \n"
    " vary with duration, as in case (a), BUT to specify the duration       \n"
    " at which the amplitude goes to 1.  This desideratum has now been      \n"
    " implemented, and provides the case below:                             \n"
    "                                                                       \n"
    "   (a1) 'dmUBLOCK(-X)' = set the amplitude to be 1 for a duration      \n"
    "                         of 'X' seconds; e.g., 'dmBLOCK(-5)' means     \n"
    "                         that a stimulus with duration 5 gets          \n"
    "                         amplitude 1, shorter durations get amplitudes \n"
    "                         smaller than 1, and longer durations get      \n"
    "                         amplitudes larger than 1.                     \n"
    "                    -->> Please note that 'dmBLOCK(-X)' is NOT the     \n"
    "                         same as this case (a1), and in fact it        \n"
    "                         has no meaning.                               \n"
    "                                                                       \n"
    " I hope this clarifies things and makes your life simpler, happier,    \n"
    " and more carefree. (If not, please blame Gang Chen, not me.)          \n"
    "                                                                       \n"
    " An example to clarify the difference between these cases:             \n"
    "    3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 3 \\\n"
    "                 -stim_times_AM1 1 q.1D 'dmUBLOCK'      \\\n"
    "                 -stim_times_AM1 2 q.1D 'dmUBLOCK(1)'   \\\n"
    "                 -stim_times_AM1 3 q.1D 'dmUBLOCK(-4)'  \\\n"
    "                 -x1D stdout: |                         \\\n"
    "     1dplot -stdin -thick                               \\\n"
    "            -ynames 'dmUBLOCK' 'dmUB(1)' 'dmUB(-4)'                    \n"
    " where file q.1D contains the single line                              \n"
    "   10:1 60:2 110:4 160:10 210:20 260:30                                \n"
    " Note how the 'dmUBLOCK(-4)' curve (green) peaks at 1 for the 3rd      \n"
    " stimulus, and peaks at larger values for the later (longer) blocks.   \n"
    " Whereas the 'dmUBLOCK' curve (black) peaks at 1 at only the longest   \n"
    " blocks, and the 'dmUBLOCK(1)' curve (red) peaks at 1 for ALL blocks.  \n"
    " ********************************************************************* \n"
    "                                                                       \n"
    "[-stim_times_FSL k tname Rmodel]                                       \n"
    "   This option allows you to input FSL-style 3-column timing files,    \n"
    "   where each line corresponds to one stimulus event/block; the        \n"
    "   line '40 20 1' means 'stimulus starts at 40 seconds, lasts for      \n"
    "   20 seconds, and is given amplitude 1'.  Since in this format,       \n"
    "   each stimulus can have a different duration and get a different     \n"
    "   response amplitude, the 'Rmodel' must be one of the 'dm'            \n"
    "   duration-modulated options above ['dmUBLOCK(1)' is probably the     \n"
    "   most useful].  The amplitude modulation is taken to be like         \n"
    "   '-stim_times_AM1', where the given amplitude in the 'tname' file    \n"
    "   multiplies the basic response shape.                                \n"
    " *** We DO NOT advocate the use of this '_FSL' option, but it's here   \n"
    "     to make some scripting easier for some (unfortunate) people.      \n"
    " *** The results of 3dDeconvolve (or 3dREMLfit) cannot be expected     \n"
    "     to be exactly the same as FSL FEAT, since the response model      \n"
    "     shapes are different, among myriad other details.                 \n"
    " *** You can also use '-stim_times_FS1' to indicate that the           \n"
    "     amplitude factor in the 'tname' file should be ignored and        \n"
    "     replaced with '1' in all cases.                                   \n"
    " *** FSL FEAT only analyzes contiguous time series -- nothing like     \n"
    "     '-concat' allowing for multiple EPI runs is possible in FSL       \n"
    "     (AFAIK).  So the FSL stimulus time format doesn't allow for       \n"
    "     this possibility.  In 3dDeconvolve, you can get around this       \n"
    "     problem by using a line consisting of '* * *' to indicate the     \n"
    "     break between runs, as in the example below:                      \n"
    "         1 2 3                                                         \n"
    "         4 5 6                                                         \n"
    "         * * *                                                         \n"
    "         7 8 9                                                         \n"
    "     that indicates 2 runs, the first of which has 2 stimuli and       \n"
    "     the second of which has just 1 stimulus.  If there is a run       \n"
    "     that has NO copies of this type of stimulus, then you would       \n"
    "     use two '* * *' lines in succession.                              \n"
    "     Of course, a file using the '* * *' construction will NOT be      \n"
    "     compatible with FSL!                                              \n"
    "                                                                       \n"
    "[-stim_times_IM k tname Rmodel]                                        \n"
    "   Similar, but each separate time in 'tname' will get a separate      \n"
    "   regressor; 'IM' means 'Individually Modulated' -- that is, each     \n"
    "   event will get its own amplitude estimated.  Presumably you will    \n"
    "   collect these many amplitudes afterwards and do some sort of        \n"
    "   statistics or analysis on them.                                     \n"
    " *N.B.: Each time in the 'tname' file will get a separate regressor.   \n"
    "        If some time is outside the duration of the imaging run(s),    \n"
    "        or if the response model for that time happens to hit only     \n"
    "        censored-out data values, then the corresponding regressor     \n"
    "        will be all zeros.  Normally, 3dDeconvolve will not run        \n"
    "        if the matrix has any all zero columns.  To carry out the      \n"
    "        analysis, use the '-allzero_OK' option.  Amplitude estimates   \n"
    "        for all zero columns will be zero, and should be excluded      \n"
    "        from any subsequent analysis.  (Probably you should fix the    \n"
    "        times in the 'tname' file instead of using '-allzero_OK'.)     \n"
    "                                                                       \n"
    "[-global_times]                                                        \n"
    "[-local_times]                                                         \n"
    "   By default, 3dDeconvolve guesses whether the times in the 'tname'   \n"
    "   files for the various '-stim_times' options are global times        \n"
    "   (relative to the start of run #1) or local times (relative to       \n"
    "   the start of each run).  With one of these options, you can force   \n"
    "   the times to be considered as global or local for '-stim_times'     \n"
    "   options that are AFTER the '-local_times' or '-global_times'.       \n"
    " ** Using one of these options (most commonly, '-local_times') is      \n"
    "    VERY highly recommended.                                           \n"
    "                                                                       \n"
    "[-stim_times_millisec]                                                 \n"
    " This option scales all the times in any '-stim_times_*' option by     \n"
    " 0.001; the purpose is to allow you to input the times in ms instead   \n"
    " of in s.  This factor will be applied to ALL '-stim_times' inputs,    \n"
    " before or after this option on the command line.  This factor will    \n"
    " be applied before -stim_times_subtract, so the subtraction value      \n"
    " (if present) must be given in seconds, NOT milliseconds!              \n"
    "                                                                       \n"
    "[-stim_times_subtract SS]                                              \n"
    " This option means to subtract 'SS' seconds from each time encountered \n"
    " in any '-stim_times*' option.  The purpose of this option is to make  \n"
    " it simple to adjust timing files for the removal of images from the   \n"
    " start of each imaging run.  Note that this option will be useful      \n"
    " only if both of the following are true:                               \n"
    "  (a) each imaging run has exactly the same number of images removed   \n"
    "  (b) the times in the 'tname' files were not already adjusted for     \n"
    "      these image removal (i.e., the times refer to the image runs     \n"
    "      as acquired, not as input to 3dDeconvolve).                      \n"
    " In other words, use this option with understanding and care!          \n"
    " ** Note that the subtraction of 'SS' applies to ALL '-stim_times'     \n"
    "    inputs, before or after this option on the command line!           \n"
    " ** And it applies to global times and local times alike!              \n"
    " ** Any time (thus subtracted) below 0 will be ignored, as falling     \n"
    "    before the start of the imaging run.                               \n"
    " ** This option, and the previous one, are simply for convenience, to  \n"
    "    help you in setting up your '-stim_times*' timing files from       \n"
    "    whatever source you get them.                                      \n"
    "                                                                       \n"
    "[-basis_normall a]                                                     \n"
    "   Normalize all basis functions for '-stim_times' to have             \n"
    "   amplitude 'a' (must have a > 0).  The peak absolute value           \n"
    "   of each basis function will be scaled to be 'a'.                    \n"
    "   NOTES:                                                              \n"
    "    * -basis_normall only affect -stim_times options that              \n"
    "        appear LATER on the command line                               \n"
    "    * The main use for this option is for use with the                 \n"
    "        'EXPR' basis functions.                                        \n"
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
    "******* General linear test (GLT) options *******                      \n"
    "                                                                       \n"
    "-num_glt num         num = number of general linear tests (GLTs)       \n"
    "                       (0 <= num)   [default: num = 0]                 \n"
    "                  **N.B.: You only need this option if you have        \n"
    "                          more than 10 GLTs specified; the program     \n"
    "                          has built-in space for 10 GLTs, and          \n"
    "                          this option is used to expand that space.    \n"
    "                          If you use this option, you should place     \n"
    "                          it on the command line BEFORE any of the     \n"
    "                          other GLT options.                           \n"
    "[-glt s gltname]     Perform s simultaneous linear tests, as specified \n"
    "                       by the matrix contained in file 'gltname'       \n"
    "[-glt_label k glabel]  glabel = label for kth general linear test      \n"
    "[-gltsym gltname]    Read the GLT with symbolic names from the file    \n"
    "                       'gltname'; see the document below for details:  \n"
    "  https://afni.nimh.nih.gov/pub/dist/doc/misc/Decon/DeconSummer2004.html\n"
#if 0
    "[-TR_irc dt]                                                           \n"
    "   Use 'dt' as the stepsize for computation of integrals in -IRC_times \n"
    "   options.  Default is to use value given in '-TR_times'.             \n"
#endif
    "                                                                       \n"
    "******* Options to create 3D+time datasets *******                     \n"
    "                                                                       \n"
    "[-iresp k iprefix]   iprefix = prefix of 3D+time output dataset which  \n"
    "                       will contain the kth estimated impulse response \n"
    "[-tshift]            Use cubic spline interpolation to time shift the  \n"
    "                       estimated impulse response function, in order to\n"
    "                       correct for differences in slice acquisition    \n"
    "                       times. Note that this effects only the 3D+time  \n"
    "                       output dataset generated by the -iresp option.  \n"
    "             **N.B.: This option only applies to the 'old' style of    \n"
    "                     deconvolution analysis.  Do not use this with     \n"
    "                     -stim_times analyses!                             \n"
    "[-sresp k sprefix]   sprefix = prefix of 3D+time output dataset which  \n"
    "                       will contain the standard deviations of the     \n"
    "                       kth impulse response function parameters        \n"
    "[-fitts  fprefix]    fprefix = prefix of 3D+time output dataset which  \n"
    "                       will contain the (full model) time series fit   \n"
    "                       to the input data                               \n"
    "[-errts  eprefix]    eprefix = prefix of 3D+time output dataset which  \n"
    "                       will contain the residual error time series     \n"
    "                       from the full model fit to the input data       \n"
    "[-TR_times dt]                                                         \n"
    "   Use 'dt' as the stepsize for output of -iresp and -sresp file       \n"
    "   for response models generated by '-stim_times' options.             \n"
    "   Default is same as time spacing in the '-input' 3D+time dataset.    \n"
    "   The units here are in seconds!                                      \n"
    "                                                                       \n"
    "**** Options to control the contents of the output bucket dataset **** \n"
    "                                                                       \n"
    "[-fout]            Flag to output the F-statistics for each stimulus   \n"
    "                    ** F tests the null hypothesis that each and every \n"
    "                       beta coefficient in the stimulus set is zero    \n"
    "                    ** If there is only 1 stimulus class, then its     \n"
    "                       '-fout' value is redundant with the Full_Fstat  \n"
    "                       computed for all stimulus coefficients together.\n"
    "[-rout]            Flag to output the R^2 statistics                   \n"
    "[-tout]            Flag to output the t-statistics                     \n"
    "                    ** t tests a single beta coefficient against zero  \n"
    "                    ** If a stimulus class has only one regressor, then\n"
    "                       F = t^2 and the F statistic is redundant with t.\n"
    "[-vout]            Flag to output the sample variance (MSE) map        \n"
    "[-nobout]          Flag to suppress output of baseline coefficients    \n"
    "                     (and associated statistics) [** DEFAULT **]       \n"
    "[-bout]            Flag to turn on output of baseline coefs and stats. \n"
    "                    ** Will make the output dataset larger.            \n"
    "[-nocout]          Flag to suppress output of regression coefficients  \n"
    "                     (and associated statistics)                       \n"
    "                    ** Useful if you just want GLT results.            \n"
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
    "[-noFDR]           Don't compute the statistic-vs-FDR curves for the   \n"
    "                     bucket dataset.                                   \n"
    "                     [same as 'setenv AFNI_AUTOMATIC_FDR NO']          \n"
    "                                                                       \n"
    "[-xsave]           Flag to save X matrix into file bprefix.xsave       \n"
    "                     (only works if -bucket option is also given)      \n"
    "[-noxsave]         Don't save X matrix [this is the default]           \n"
    "[-cbucket cprefix] Save the regression coefficients (no statistics)    \n"
    "                     into a dataset named 'cprefix'.  This dataset     \n"
    "                     will be used in a -xrestore run instead of the    \n"
    "                     bucket dataset, if possible.                      \n"
    "                ** Also, the -cbucket and -x1D output can be combined  \n"
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
    "                    as scaled shorts [** now the default **]           \n"
    "[-short]            Write output as scaled shorts [no longer default]  \n"
    "                                                                       \n"
    "***** The following options control miscellanous outputs *****         \n"
    "                                                                       \n"
    "[-quiet]             Flag to suppress most screen output               \n"
    "[-xout]              Flag to write X and inv(X'X) matrices to screen   \n"
    "[-xjpeg filename]    Write a JPEG file graphing the X matrix           \n"
    "                     * If filename ends in '.png', a PNG file is output\n"
    "[-x1D filename]      Save X matrix to a .xmat.1D (ASCII) file [default]\n"
    "                    ** If 'filename' is 'stdout:', the file is written \n"
    "                       to standard output, and could be piped into     \n"
    "                       1dplot (some examples are given earlier).       \n"
    "                     * This can be used for quick checks to see if your\n"
    "                       inputs are setting up a 'reasonable' matrix.    \n"
    "[-nox1D]             Don't save X matrix [a very bad idea]             \n"
    "[-x1D_uncensored ff] Save X matrix to a .xmat.1D file, but WITHOUT     \n"
    "                     ANY CENSORING.  Might be useful in 3dSynthesize.  \n"
    "[-x1D_regcensored f] Save X matrix to a .xmat.1D file with the         \n"
    "                     censoring imposed by adding 0-1 columns instead   \n"
    "                     excising the censored rows.                       \n"
    "[-x1D_stop]          Stop running after writing .xmat.1D files.        \n"
    "                     * Useful for testing, or if you are going to      \n"
    "                       run 3dREMLfit instead -- that is, you are just  \n"
    "                       using 3dDeconvolve to set up the matrix file.   \n"
    "[-progress n]        Write statistical results for every nth voxel     \n"
    "                     * To let you know that something is happening!    \n"
    "[-fdisp fval]        Write statistical results to the screen, for those\n"
    "                       voxels whose full model F-statistic is > fval   \n"
    "[-help]              Oh go ahead, try it!                              \n"
    );

#ifdef PROC_MAX
    printf( "\n"
            "**** Multiple CPU option (local CPUs only, no networking) ****\n"
            "\n"
            " -jobs J   Run the program with 'J' jobs (sub-processes).\n"
            "             On a multi-CPU machine, this can speed the\n"
            "             program up considerably.  On a single CPU\n"
            "             machine, using this option would be silly.\n"
            "         * J should be a number from 1 up to the\n"
            "             number of CPUs sharing memory on the system.\n"
            "         * J=1 is normal (single process) operation.\n"
            "         * The maximum allowed value of J is %d.\n"
            "         * Unlike other parallelized AFNI programs, this one\n"
            "             does not use OpenMP; it directly uses fork()\n"
            "             and shared memory to run multiple processes.\n"
            "         * For more information on parallelizing, see\n"
            "           https://afni.nimh.nih.gov/afni/doc/misc/afni_parallelize\n"
            "         * Also use -mask or -automask to get more speed; cf. 3dAutomask.\n"
          , PROC_MAX ) ;

    printf( "\n"
            "-virtvec   To save memory, write the input dataset to a temporary file\n"
            "           and then read data vectors from it only as needed.  This option\n"
            "           is for Javier and will probably not be useful for anyone else.\n"
            "           And it only takes effect if -jobs is greater than 1.\n"
          ) ;
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

  PRINT_COMPILE_DATE; exit(0);
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
  option_data->force_TR = 0.0;  /* 18 Aug 2008 */
  option_data->coldat   = NULL; /* 06 Mar 2007 */

  option_data->tcat_noblock = 0 ; /* 06 Jan 2011 */

  option_data->xjpeg_filename = NULL ;  /* 21 Jul 2004 */
  option_data->x1D_filename   = NULL ;
  option_data->x1D_unc        = NULL ;
  option_data->x1D_regcen     = NULL ;  /* 16 Aug 2019 */
  option_data->nox1D          = 0 ;
  option_data->x1D_stop       = 0 ;

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

  if( AFNI_noenv("AFNI_SKIP_SATCHECK") ) dont_do_satcheck = 0; /* 23 Dec 2011 */
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
  option_data->num_stimts = num_stimts;


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

void extend_stim_options( DC_options *option_data , int nadd )
{
  int is , num_old , num_new ;

ENTRY("extend_stim_options") ;

  if( option_data == NULL || nadd <= 0 ) EXRETURN ;


  num_old = option_data->num_stimts ;
  num_new = num_old + nadd ;

  option_data->stim_filename  = (char **)realloc(option_data->stim_filename ,sizeof(char *)*num_new);
  option_data->stim_label     = (char **)realloc(option_data->stim_label    ,sizeof(char *)*num_new);
  option_data->stim_base      = (int *)  realloc(option_data->stim_base     ,sizeof(int)   *num_new);
  option_data->stim_minlag    = (int *)  realloc(option_data->stim_minlag   ,sizeof(int)   *num_new);
  option_data->stim_maxlag    = (int *)  realloc(option_data->stim_maxlag   ,sizeof(int)   *num_new);
  option_data->stim_nptr      = (int *)  realloc(option_data->stim_nptr     ,sizeof(int)   *num_new);
  option_data->iresp_filename = (char **)realloc(option_data->iresp_filename,sizeof(char *)*num_new);
  option_data->sresp_filename = (char **)realloc(option_data->sresp_filename,sizeof(char *)*num_new);
  option_data->slice_base     = (int *)  realloc(option_data->slice_base    ,sizeof(int)   *num_new);

  basis_stim  = (basis_expansion **)realloc(basis_stim ,sizeof(basis_expansion *)*num_new);
  basis_times = (MRI_IMAGE **)      realloc(basis_times,sizeof(MRI_IMAGE *)      *num_new);
  basis_vect  = (MRI_IMAGE **)      realloc(basis_vect ,sizeof(MRI_IMAGE *)      *num_new);

  for (is = num_old;  is < num_new;  is++)
    {
      option_data->stim_filename[is] = NULL;
      option_data->stim_label[is] = malloc (sizeof(char)*THD_MAX_NAME);
      sprintf (option_data->stim_label[is], "Stim#%d", is+1);

      option_data->stim_base[is]    = 0;
      option_data->stim_minlag[is]  = 0;
      option_data->stim_maxlag[is]  = 0;
      option_data->stim_nptr[is]    = 1;
      option_data->slice_base[is]   = 0;

      option_data->iresp_filename[is] = NULL;
      option_data->sresp_filename[is] = NULL;

      basis_stim [is] = NULL ;
      basis_times[is] = NULL ;
      basis_vect [is] = NULL ;
    }

   option_data->num_stimts = num_new ;

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
  float ttmax=big_time ;   /* 28 Aug 2015 */
  float gtmax=big_time ;   /* same, for global timing  19 Jul 2017 [rickr] */

  /*-- addto the arglist, if user wants to --*/
  { int new_argc ; char **new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }


  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv);


  /*----- initialize the input options -----*/
  initialize_options (option_data);

       if( AFNI_yesenv("AFNI_FLOATIZE") ) floatout = 1 ;  /* 17 Jan 2008 */
  else if( AFNI_yesenv("AFNI_SHORTIZE") ) floatout = 0 ;  /* 15 Jun 2010 */

  if( AFNI_yesenv("AFNI_3dDeconvolve_GOFORIT") ) goforit++ ; /* 07 Mar 2007 */

  if( argc < 2 ) {           /* for us lazy people   18 Jul 2013 [rickr] */
    display_help_menu(1);
    exit(0);
  }

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {
      /*----- does user request help menu? -----*/
      if (strcmp(argv[nopt], "-h") == 0 || strcmp(argv[nopt], "-help") == 0) {
         display_help_menu(strlen(argv[nopt])>3 ? 2:1);
         exit(0);
      }

      if( strcmp(argv[nopt],"-OK") == 0 ){ nopt++; continue; } /* 14 Jul 2004 */

      if( strcmp(argv[nopt],"-notrans") == 0 || strcmp(argv[nopt],"-nosat") == 0 ){
        dont_do_satcheck = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-trans") == 0 || strcmp(argv[nopt],"-sat") == 0 ){
        dont_do_satcheck = 0 ; nopt++ ; continue ;
      }

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

      if( strcasecmp(argv[nopt],"-allzero_OK") == 0 ){  /* 30 May 2007 */
        allzero_OK++ ; use_psinv = 1 ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-sing",5) == 0 ){  /* 13 Aug 2004 */
        show_singvals = 1 ; option_data->nocond = 0 ;
        nopt++ ; continue ;
      }

      /*-----   -xjpeg filename  ------*/
      if (strcmp(argv[nopt], "-xjpeg") == 0)   /* 21 Jul 2004 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -xjpeg ");
        option_data->xjpeg_filename = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->xjpeg_filename);
        strcpy (option_data->xjpeg_filename, argv[nopt]);
        if( !STRING_HAS_SUFFIX_CASE(option_data->xjpeg_filename,".jpg") &&
            !STRING_HAS_SUFFIX_CASE(option_data->xjpeg_filename,".png")   )
          strcat( option_data->xjpeg_filename , ".jpg" ) ;
        nopt++; continue;
      }

      /*-----   -x1D filename  ------*/
      if (strcmp(argv[nopt], "-x1D") == 0)   /* 28 Mar 2006 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -x1D ");
        option_data->x1D_filename = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->x1D_filename);
        strcpy (option_data->x1D_filename, argv[nopt]);
        if( strstr(option_data->x1D_filename,"1D") == NULL )
          strcat( option_data->x1D_filename , ".xmat.1D" ) ;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nox1D") == 0 ){  /* 20 Mar 2007 */
        option_data->nox1D = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-x1D_stop") == 0 ){  /* 20 Mar 2007 */
        option_data->x1D_stop = 1; option_data->nox1D = 0; nopt++; continue;
      }

      /*-----   -x1D_unc filename  ------*/
      if (strncmp(argv[nopt], "-x1D_unc",8) == 0)   /* 25 Jun 2007 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -x1D_uncensored ");
        option_data->x1D_unc = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->x1D_unc);
        strcpy (option_data->x1D_unc, argv[nopt]);
        if( strstr(option_data->x1D_unc,"1D") == NULL )
          strcat( option_data->x1D_unc , ".xmat.1D" ) ;
        nopt++; continue;
      }

      /*-----   -x1D_regcen filename  ------*/
      if (strncmp(argv[nopt], "-x1D_regcen",11) == 0)   /* 16 Aug 2019 */
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -x1D_regcensored ");
        option_data->x1D_regcen = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->x1D_regcen);
        strcpy (option_data->x1D_regcen, argv[nopt]);
        if( strstr(option_data->x1D_regcen,"1D") == NULL )
          strcat( option_data->x1D_regcen , ".xmat.1D" ) ;
        nopt++; continue;
      }

      /*-----    06 Jan 2011    -----*/
      if( strcmp(argv[nopt],"-noblock") == 0 ){
        option_data->tcat_noblock = 1 ; nopt++ ; continue ;
      }

      /*-----   -input filename   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
      {
          int iopt , slen ;
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -input ");
#if 0
        option_data->input_filename = malloc (sizeof(char)*ALEN(nopt));
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
        { THD_3dim_dataset *dset ; int lmax ;
          dset = THD_open_dataset (option_data->input_filename);
          CHECK_OPEN_ERROR(dset,option_data->input_filename) ;
          lmax = DSET_NVALS(dset) ;
          /* compute gtmax, as ttmax for global timing  19 Jul 2017 [rickr] */
          /* (init to lmax if not in TCAT case)          4 Dec 2017 [rickr] */
          gtmax = lmax;
          if( DSET_IS_TCAT(dset) && !option_data->tcat_noblock ){
            int it ;
            gtmax = 0.0;
            for( lmax=2,it=0 ; it < dset->tcat_num ; it++ ) {
              lmax = MAX( lmax , dset->tcat_len[it] ) ;
              /* accumulate time points, scale by TR later */
              gtmax += dset->tcat_len[it];
            }
          }

          /* if force_TR is set, apply it           5 Jul 2017 [rickr] */
          if( option_data->force_TR > 0.0 ) {
            ttmax = lmax * option_data->force_TR ;
            gtmax *= option_data->force_TR ; /* scale NT by TR */
          } else {
            ttmax = lmax * DSET_TR(dset) ;
            gtmax *= DSET_TR(dset) ;         /* scale NT by TR */
            if( DSET_TR(dset) == 0.0 )       /* 23 Oct 2017 [rickr] */
              WARNING_message("-input dataset seems to have TR = 0.0");
          }
          DSET_delete(dset) ;
        }

        continue;
      }

      /*-----  -STATmask  fname  -----*/

      if( strcasecmp(argv[nopt],"-STATmask") == 0 ||
          strcasecmp(argv[nopt],"-FDRmask")  == 0   ){
        nopt++ ;
        if( statmask != NULL ) DC_error("can't use -STATmask twice") ;
        if( nopt     >= argc ) DC_error("need argument after -STATmask") ;
        statmask_name = strdup(argv[nopt]) ;
        statmask = THD_create_mask_from_string(statmask_name) ;
        if( statmask == NULL ){
          WARNING_message("-STATmask being ignored: can't use it") ;
          free(statmask_name) ; statmask_name = NULL ;
        }
        nopt++ ; continue ;
      }

      /*-----   -mask filename   -----*/
      if (strcmp(argv[nopt], "-mask") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -mask ");
        if( option_data->automask ) DC_error("can't use -mask AND -automask!") ;
        option_data->mask_filename = malloc (sizeof(char)*ALEN(nopt));
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
          malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->input1D_filename);
        strcpy (option_data->input1D_filename, argv[nopt]);
        nopt++;
        continue;
      }

      /*-----   -force_TR  -------*/
      if (strcmp(argv[nopt], "-force_TR") == 0)  /* 18 Aug 2008 */
      {
        /* if ttmax is alreay set (from -input), suggest putting -force_TR
         * earlier (this is for _IM, at least)          5 Jul 2017 [rickr] */
        if (ttmax < big_time)
           ERROR_exit("please apply -force_TR before -input\n"
                      "   (or use 3drefit so it is not needed)") ;

        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -force_TR ");
        option_data->force_TR = (float)strtod(argv[nopt++],NULL) ;
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

      /*-----   -noFDR  [23 Jan 2008]  -----*/

      if( strcmp(argv[nopt],"-noFDR") == 0 ){
        do_FDR = 0 ; nopt++ ; continue ;
      }

      /*-----   -censor filename   -----*/
      if (strcmp(argv[nopt], "-censor") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -censor ");
        if( option_data->censor_filename != NULL )
          WARNING_message("second -censor option replacing first one!") ;
        option_data->censor_filename =
          malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->censor_filename);
        strcpy (option_data->censor_filename, argv[nopt]);
        nopt++;
        continue;
      }

      /*-----  -CENSOR clist -----*/

      if( strncmp(argv[nopt],"-CENSOR",7)   == 0 ||
          strncmp(argv[nopt],"-censorTR",9) == 0   ){   /* RWCox - 01 Mar 2007 */

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
                ERROR_message("-CENSORTR %s -- run index '%d' is bad! [nopt=%d]",nsar->str[ns],r,nopt);
                nerr++ ; continue ;
              }
            }
            cpt = dpt+1 ;  /* skip to character after ':' */
            if( *cpt == '\0' ){  /* skip out */
              ERROR_message("-CENSORTR %s -- no data after run index! [nopt=%d]",nsar->str[ns],nopt);
              nerr++ ; continue ;
            }
          }
          a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
          if( a < 0 ){  /* skip out */
            ERROR_message("-CENSORTR %s -- time index '%d' is bad! [nopt=%d]",nsar->str[ns],a,nopt);
            nerr++ ; continue ;
          }
          if( *dpt == '\0' ){  /* no second number */
            b = a ;
          } else {             /* get second number */
            for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
            b = (int)strtol(dpt,NULL,10) ;
            if( b < a || b < 0 ){  /* skip out */
              ERROR_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad! [nopt=%d]",
                            nsar->str[ns],a,b,nopt);
              nerr++ ; continue ;
            }
          }
          abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                              sizeof(int_triple)*(num_CENSOR+1) );
          rab.i = r; rab.j = a; rab.k = b; abc_CENSOR[num_CENSOR++] = rab ;
        } /* end of loop over -CENSORTR strings */
        if( nerr > 0 ) ERROR_exit("Can't proceed after -CENSORTR errors! [nopt=%d]",nopt) ;
        NI_delete_str_array(nsar) ; free(src) ;
        continue ;  /* next option */
      }

      /*-----   -concat filename   -----*/
      if (strcmp(argv[nopt], "-concat") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -concat ");
        option_data->concat_filename =
          malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->concat_filename);
        strcpy (option_data->concat_filename, argv[nopt]);
        nopt++;
        continue;
      }


      /*-----   -nodata [NT [TR]]  -----*/
      if (strcmp(argv[nopt], "-nodata") == 0){
        option_data->nodata = 1; nopt++;

        /* 27 Apr 2005: check for additional numeric values */

        if( nopt < argc && isdigit(argv[nopt][0]) ){  /* get NT */
          option_data->nodata_NT = (int)strtol(argv[nopt++],NULL,10) ;
          if( nopt < argc && isdigit(argv[nopt][0]) ){  /* also get TR */
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

      /*-----   -dofsub ddd   -----*/

      if( strcmp(argv[nopt],"-dofsub") == 0 ){
        nopt++ ;
        if (nopt >= argc)  DC_error ("need argument after -dofsub ");
        dofsub = (int)strtod(argv[nopt++],NULL) ;
        if( dofsub < 0 ) WARNING_message("-dofsub value is negative!") ;
        continue ;
      }

      /*-----   -polort num  -----*/
      if (strcmp(argv[nopt], "-polort") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need argument after -polort ");
        if( toupper(argv[nopt][0]) == 'A' ){  /* 16 Mar 2006: Automatic */
          ival = -666 ;
        } else {
          char *qpt ;
          ival = (int)strtod(argv[nopt],&qpt) ;
          if( *qpt != '\0' ) WARNING_message("Illegal non-numeric value after -polort") ;
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
        nopt++; continue;
      }

      if (strncmp(argv[nopt],"-verb",5) == 0){  /* 01 Mar 2007 */
        verb++ ; nopt++ ; continue ;
      }

      /*-----   -float, etc.   -----*/
      if( strncmp(argv[nopt],"-float",6) == 0 ){    /* 13 Mar 2007 */
        floatout = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-short") == 0 ){
        if( floatout < 2 ) floatout = 0 ;   /* 2 ==> floatout was forced on */
        nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-datum") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -datum");
             if( strcmp(argv[nopt],"float") == 0 ) floatout = 1 ;
        else if( strcmp(argv[nopt],"short") == 0 ) floatout = 0 ;
        else              DC_error("illegal type name after -datum") ;
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
          ERROR_exit("-TR_times '%s' is illegal [nopt=%d]\n",argv[nopt],nopt) ;
        nopt++ ; continue ;
      }

      /*-----  -TR_irc irc_dt [08 Sep 2004]  -----*/
      if( strcmp(argv[nopt],"-TR_irc") == 0 ){
        nopt++ ;
        if( nopt >= argc ) DC_error("need argument after -TR_irc") ;
        irc_dt = -1.0 ; sscanf( argv[nopt] , "%f" , &irc_dt ) ;
        if( irc_dt <= 0.0f )
          ERROR_exit("-TR_irc '%s' is illegal [nopt=%d]",argv[nopt],nopt) ;
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

      /*-----  -local_times AND -global_times [16 Nov 2007]  -----*/

      if( strcmp(argv[nopt],"-local_times") == 0 ){
        basis_timetype = LOCAL_TIMES ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-global_times") == 0 ){
        basis_timetype = GLOBAL_TIMES ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-guess_times") == 0 ){
        basis_timetype = GUESS_TIMES ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stim_times_subtract") == 0 ){  /** 24 Mar 2009 **/
        char *ept ;
        if( nopt+1 >= argc ) ERROR_exit("need 1 argument after %s [nopt=%d]",argv[nopt],nopt) ;
        stime_sub = (float)strtod(argv[++nopt],&ept) ;
        if( ept == argv[nopt] ){
          WARNING_message("%s %s doesn't make sense!",argv[nopt-1],argv[nopt]) ;
          stime_sub = 0.0f ;
        }
        nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stim_times_millisec") == 0 ){  /** 24 Mar 2009 **/
        stime_fac = 0.001f ; nopt++ ; continue ;
      }

      /*-----  -stim_times k sname rtype [10 Aug 2004]  -----*/

      if( strncmp(argv[nopt],"-stim_times",11) == 0 ){
        char *suf = argv[nopt]+11 , *sopt=argv[nopt] , *model=NULL ;
        int nc=0,vdim=0,vmod=0 ; MRI_IMAGE *tim ;
        int do_FSL=0 ;

        nopt++ ;

        /* check for errors */

        if( *suf != '\0'            &&
            strcmp(suf,"_AM1") != 0 &&
            strcmp(suf,"_AM2") != 0 &&
            strcmp(suf,"_AMx") != 0 &&
            strcmp(suf,"_IM" ) != 0 &&
            strcmp(suf,"_FSL") != 0 &&
            strcmp(suf,"_FS1") != 0   )
          ERROR_exit("Unrecognized -stim_times variant: '%s' [nopt=%d]",sopt,nopt) ;

        do_FSL = ( strcmp(suf,"_FSL") == 0 || strcmp(suf,"_FS1") == 0 ) ;

        if( nopt+2 >= argc )
          ERROR_exit("need 3 arguments after %s [nopt=%d]",sopt,nopt) ;

        /* get stim number */

        ival = -1 ; sscanf( argv[nopt] , "%d" , &ival ) ;

        if( ival < 1 || ival > option_data->num_stimts )
          ERROR_exit("'-stim_times %d' value out of range 1..%d [nopt=%d]\n",
                     ival , option_data->num_stimts , nopt ) ;

        k = ival-1 ; nopt++ ;                         /* k = stim array index */

        /* get stim timing image */

        if( option_data->stim_filename[k] != NULL )
          ERROR_exit("'%s %d' trying to over-write previous stimulus?! [nopt=%d]",
                     sopt , ival , nopt ) ;

        option_data->stim_filename[k] = strdup( argv[nopt] ) ;

        basis_times[k] = mri_read_ascii_ragged_fvect(argv[nopt],basis_filler,0);

        if( basis_times[k] == NULL || basis_times[k]->vdim <= 0 )
          ERROR_exit("'%s %d' can't read file '%s' [nopt=%d]\n",
                     sopt , ival , argv[nopt] , nopt ) ;

        if( do_FSL && basis_times[k]->vdim > 1 )
          ERROR_exit("'%s %d' file '%s' does not have correct FSL format [nopt=%d]",
                     sopt , ival , argv[nopt] , nopt ) ;

        if( basis_times[k]->vdim == 1 ){      /* scalar image */
          basis_times[k]->vdim = 0 ;
          basis_times[k]->kind = MRI_float ;  /* mark as pure float type */
          tim = basis_times[k] ;
        } else {
          tim = mri_fvect_subimage( basis_times[k] , 0 ) ; /* extract times */
        }

        if( do_FSL && tim->nx > 1 ){
          MRI_IMAGE *qim = mri_new(1,tim->ny,MRI_float) ;
          float     *qar = MRI_FLOAT_PTR(qim) , *tar = MRI_FLOAT_PTR(tim) ;
          int qq ;
          for( qq=0 ; qq < tim->ny ; qq++ ) qar[qq] = tar[qq*tim->nx] ;
          tim = qim ;
        }

        /* check number of reasonable times */

        /* allow for global timing    19 Jul 2017 [rickr] */
        if( basis_timetype == GLOBAL_TIMES )
           nc = mri_counter( tim , 0.0f , gtmax ) ;
        else
           nc = mri_counter( tim , 0.0f , ttmax ) ;

        if( tim != basis_times[k] ) mri_free(tim) ;

        if( nc == 0 ) { /* 0 is okay, < 0 is not   26 Jul 2007 [rickr] */
          WARNING_message("'%s %d' didn't read any good times from file '%s'",
                          sopt , ival , argv[nopt] ) ;
          /* but 0 might be a symptom of something bad  23 Oct 2017 [rickr] */
          if( (basis_timetype == GLOBAL_TIMES && gtmax == 0.0) ||
              (basis_timetype != GLOBAL_TIMES && ttmax == 0.0) )
             WARNING_message("run time seen as 0.0, verify TR and NT");
        } else if( nc < 0 )
          ERROR_exit("'%s %d' couldn't read valid times from file '%s' [nopt=%d]",
                     sopt , ival , argv[nopt] , nopt ) ;

        /** case: 1 number per time point: -stim_times **/

        if( *suf == '\0' ){

          if( basis_times[k]->vdim > 0 )
            ERROR_exit("'%s %d' file '%s' has %d auxiliary values per time point [nopt=%d]",
                       sopt , ival , argv[nopt] , basis_times[k]->vdim-1 , nopt ) ;

        /** case: 1 or more numbers per time point: -stim_times_IM **/

        } else if( strcmp(suf,"_IM") == 0 ){

          vdim = basis_times[k]->vdim ;  /* will be 0 if there is aux data */

        /** cases: multiple numbers per time point: -stim_times_AM* **/

        } else if( strncmp(suf,"_AM",3) == 0 ){

          vdim = basis_times[k]->vdim ; /* number of values per point */
          if( vdim < 2 ){                /* need at least 2 (time & amplitude) */
            if( strncmp(argv[nopt],"1D:",3) == 0 )
              ERROR_exit(
              "'%s %d' doesn't allow '1D:' type of input -- use a file [nopt=%d] :-(",
              sopt , ival , nopt ) ;
            else
              ERROR_exit(
              "'%s %d' file '%s' doesn't have any auxiliary values per time point! [nopt=%d] :-(\n%s",
              sopt , ival , argv[nopt] , nopt ,
              (nc > 0) ? " ==> You need at least 1 extra value 'married' to each stimulus start time\n"
                       : " ==> To have NO valid times, use a time of '-1' and 'marry' it as desired\n"
              ) ;
          }
          else if( vdim-1 > BASIS_MAX_VDIM ) /* over the limit */
            ERROR_exit(
              "'%s %d' file '%s' has too many auxiliary values per time point! [nopt=%d] :-(",
              sopt , ival , argv[nopt] , nopt ) ;
          else                               /* juuusst right */
            INFO_message(
              "'%s %d %s' has %d auxiliary values per time point",
              sopt , ival , argv[nopt] , vdim-1 ) ;

        } else if( do_FSL ){  /* 14 Nov 2013: FSL-like input */

          MRI_IMAGE *newbt ;

          /** mangle FSL 3 column input into a
              -stim_times_AM1 format for dmUBLOCK = time:amplitude:duration **/

          if( basis_times[k]->nx > 3 )
            WARNING_message("'%s %d' file '%s' has more than 3 columns per row -- these will be ignored [nopt=%d]",
                            sopt , ival , argv[nopt] , nopt ) ;

          newbt = convert_FSL_to_fvect( basis_times[k] , strcmp(suf,"_FS1")==0 ) ;
          if( newbt == NULL )
            ERROR_exit("'%s %d' file '%s' -- can't convert from FSL format for some reason [nopt=%d]",
                       sopt , ival , argv[nopt] , nopt ) ;

          mri_free(basis_times[k]) ; basis_times[k] = newbt ; vdim = 3 ;

        } else {  /* should not happen */

          ERROR_exit("Unknown -stim_times type of option: '%s' [nopt=%d]",sopt,nopt) ;

        }

        /** build regression model from symbolic name **/
        /** [05 Dec 2008] will also set vfun component
            = # of nonlinear function parameters from timing file **/

        nopt++ ;
        model = argv[nopt] ;
        if( do_FSL && strncmp(model,"dm",2) != 0 ){
          WARNING_message("'%s %d' file '%s' -- model '%s' is not of 'dm' type -- replacing with 'dmUBLOCK(1)'",
                          sopt , ival , argv[nopt-1] , model ) ;
          model = "dmUBLOCK(1)" ;
        }
        basis_stim[k] = basis_parser(model) ;

        if( basis_stim[k] == NULL )
          ERROR_exit("'%s %d': don't understand basis function model '%s' [nopt=%d]",
                     sopt , ival , model , nopt ) ;

        /** Allow for vfun parameters to basis function [05 Dec 2008], **/
        /** and then vmod = number of amplitude modulation parameters. **/
        /** + Note that vmod = 0 and vfun > 0 is perfectly legitimate; **/
        /** + Note that vmod+vfun+1 = vdim is required when vdim > 0;  **/
        /**   the extra '+1' is for the actual stimulus time itself!   **/
        /** + The order of 'married' parameters in the timing file is: **/
        /**     stimtime*amp#1,...,amp#vmod:parm#1,...parm#vfun        **/
        /**   where 'amp#k' is the k-th amplitude modulation parameter **/
        /**   and 'parm#j' is the j-th nonlinear parameter that will   **/
        /**   be passed to the basis function evaluator (e.g., width)  **/

        if( basis_stim[k]->vfun > 0 ){

          if( basis_stim[k]->vfun > BASIS_MAX_VFUN )  /* should not happen! */
            ERROR_exit(
              "'%s %d': basis function model '%s' uses %d parameters;\n"
              "    which is more than maximum allowed %d -- internal error!! [nopt=%d]" ,
              sopt , ival , model ,
              basis_stim[k]->vfun , BASIS_MAX_VFUN , nopt ) ;

          basis_stim[k]->vmod = vmod = vdim - 1 - basis_stim[k]->vfun ;

          if( vmod < 0 )
            ERROR_exit(
              "'%s %d': basis function model '%s' uses %d parameters,\n"
              "    more than the %d found in timing file '%s' [nopt=%d]" ,
              sopt , ival , model ,
              basis_stim[k]->vfun , vdim-1 , argv[nopt-1] , nopt ) ;

          INFO_message(
            "'%s %d': basis function model '%s' uses %d parameters,\n"
            "    out of the %d found in timing file '%s'" ,
            sopt , ival , model ,
            basis_stim[k]->vfun , vdim-1 , argv[nopt-1] ) ;

        } else if( vdim > 1 ){  /* vfun is 0, so all params are amplitudes */

          basis_stim[k]->vmod = vmod = vdim - 1 ;

        } else {                /* no auxiliary parameters */

          basis_stim[k]->vmod = vmod = 0 ;

        }

        basis_stim[k]->vdim = vdim ;

        /** number of parameters = multiple of number of basis functions **/

        basis_stim[k]->nparm = basis_stim[k]->nfunc ; /* # of params */

        if( *suf == '\0' ){                           /* 08 Mar 2007 */

          basis_stim[k]->type = BASIS_SINGLE ;  /* simplest possible case */

        } else if( strcmp(suf,"_IM") == 0 ){          /* 16 Jul 2007 */

          basis_stim[k]->type = BASIS_MODULATED_INDV;
          basis_stim[k]->nparm *= nc ;  /* nc = number of time points */
          INFO_message("'%s %d %s' will have %d regressors",
                       sopt,ival,argv[nopt-1],basis_stim[k]->nparm) ;

          if( vmod != 0 )
            ERROR_exit("'%s %d %s' has %d amplitude modulation parameters - not legal! [nopt=%d]",
                       sopt,ival,argv[nopt-1],vmod,nopt) ;
          if( basis_stim[k]->vfun > 0 && basis_stim[k]->vfun != vdim-1 )
            ERROR_exit("'%s %d %s' needs %d functional parameters but has %d [nopt=%d]",
                       sopt,ival,argv[nopt-1],basis_stim[k]->vfun,vdim-1, nopt ) ;

        } else if( strcmp(suf,"_AM1") == 0 || do_FSL ){
                                                      /* amplitude */
          basis_stim[k]->type = BASIS_MODULATED_MONO; /* modulation */

        } else if( strcmp(suf,"_AM2") == 0 || strcmp(suf,"_AMx") == 0 ){

          if( vmod == 0 ){
            basis_stim[k]->type = BASIS_MODULATED_MONO;
            WARNING_message(
               "'%s %d %s' has 0 amplitude modulation parameters;"
               "    ==> converting to be like -stim_times_AM1"    ,
               sopt,ival,argv[nopt-1] ) ;
          } else {
            basis_stim[k]->type = BASIS_MODULATED_PAIR;
            basis_stim[k]->nparm *= (vmod+1) ;      /* one for each amplitude */

            if( nopt < argc-1 && argv[nopt+1][0] == ':' ){  /* 12 Jul 2012 */
              char *mss = strdup(argv[++nopt]) , *ccc,*ddd ; int iss ;
              basis_stim[k]->modsub = (float *)malloc(sizeof(float)*vmod) ;
              for( iss=0 ; iss < vmod ; iss++ ) basis_stim[k]->modsub[iss] = basis_filler ;
              for( ccc=mss ; *ccc != '\0' ; ccc++ ){ if( *ccc == ':' || *ccc == ',' ) *ccc = ' ' ; }
              INFO_message("'%s %d %s' has modulation parameter centering string '%s'",
                           sopt,ival,argv[nopt-2],mss) ;
              for( ccc=mss,iss=0 ; iss < vmod ; iss++ ){
                for( ; isspace(*ccc) ; ccc++ ) ; /*nada*/
                if( isnumeric(*ccc) ){
                  basis_stim[k]->modsub[iss] = (float)strtod(ccc,&ddd) ;
                  ccc = ddd ;
                  ININFO_message("'%s %d %s' will subtract %g from input modulation parameter #%d",
                                 sopt,ival,argv[nopt-2],basis_stim[k]->modsub[iss],iss+1) ;
                } else {       /* skip to next nonblank location */
                  for( ccc++ ; !isspace(*ccc) ; ccc++ ) ; /*nada*/
                  ININFO_message("'%s %d %s' will subtract MEAN from modulation parameter #%d",
                                 sopt,ival,argv[nopt-2],iss+1) ;
                }
              }
            }
          }
          INFO_message("'%s %d %s' will have %d regressors",
                       sopt,ival,argv[nopt-1],basis_stim[k]->nparm) ;

        } else {

          ERROR_exit("'%s %d' -- unrecognized sub-type of '-stim_times' option! [nopt=%d]",
                     sopt , ival , nopt ) ;

        }

        basis_stim[k]->option   = strdup(sopt) ;   /* 08 Mar 2007 */
        basis_stim[k]->timetype = basis_timetype ; /* 16 Nov 2007 */
        basis_count++ ;
        nopt++ ; continue ;
      }

#if 0
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
          ERROR_exit("'-slice_base %d' trying to overwrite previous stimulus [nopt=%d]",
                     ival , nopt ) ;

        option_data->stim_filename[k] = malloc(sizeof(char)*ALEN(nopt));
        MTEST(option_data->stim_filename[k]);
        strcpy(option_data->stim_filename[k], argv[nopt]);
        option_data->slice_base[k] = 1;
        option_data->stim_base[k]  = 1;   /* mark as being in the baseline */
        nopt++;
        continue;
      }
#endif

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
            ERROR_exit("'-stim_file %d' trying to overwrite previous stimulus [nopt=%d]",
                    ival , nopt ) ;

        option_data->stim_filename[k] = malloc (sizeof(char)*ALEN(nopt));
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

      /*-----   -ortvec filename label   -----*/
      if( strcasecmp(argv[nopt],"-ortvec") == 0 ||
          strcasecmp(argv[nopt],"-inati" ) == 0   ){
        MRI_IMAGE *oim ;
        if( ++nopt >= argc-1 ) DC_error ("need 2 arguments after -ortvec") ;
        oim = mri_read_1D( argv[nopt] ) ;
        if( oim == NULL ) ERROR_exit("3dDeconvolve: -ortvec can't read file '%s'",argv[nopt]) ;
        if( oim->nx < 2 ) ERROR_exit("3dDeconvolve: -ortvec %s has nx < 2"       ,argv[nopt]) ;
        nopt++ ;
        if( argv[nopt][0] != '-' && THD_filename_ok(argv[nopt]) )
          mri_add_name( argv[nopt] , oim ) ;
        else
          mri_add_name( "ortvec" , oim ) ;
        if( ortar == NULL ) INIT_IMARR(ortar) ;
        ADDTO_IMARR(ortar,oim) ;
        nopt++ ; continue ;
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
        s = ival;  /* number of rows to read in matrix file */

        if (option_data->num_glt == 0)
          initialize_glt_options (option_data, 10);   /* default limit on GLTs */

        if (iglt+1 > option_data->num_glt)
          DC_error ("Use -num_glt option to specify number of GLTs when more than 10");

        option_data->glt_rows[iglt] = s;
        nopt++;

        option_data->glt_filename[iglt] = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->glt_filename[iglt]);
        strcpy (option_data->glt_filename[iglt], argv[nopt]);
        iglt++;

        nopt++; continue;
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
        if (option_data->glt_filename[iglt]) {  /* ZSS May 08:remove trailing '\'
                                                   or go into inf. loops */
         int ss;
         char *fdup=option_data->glt_filename[iglt];
         ss = strlen(fdup)-1;
         while (  ss >= 0 &&
                  ( fdup[ss] == '\\' || isspace(fdup[ss]) ) ) {
            fdup[ss]='\0';
            --ss;
         }
       }

        iglt++ ; nopt++ ; continue ;
      }


      /*-----   -glt_label k glabel   -----*/
      if (strcmp(argv[nopt], "-glt_label") == 0)
      {
        nopt++;
        if (nopt+1 >= argc)  DC_error ("need 2 arguments after -glt_label");

        ival = -1 ; sscanf (argv[nopt], "%d", &ival);
        if ((ival < 1) || (ival > option_data->num_glt))
          DC_error ("-glt_label k slabel   Require: 1 <= k <= num_glt");
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
          = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->iresp_filename[k]);
        strcpy (option_data->iresp_filename[k], argv[nopt]);
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
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
          = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->sresp_filename[k]);
        strcpy (option_data->sresp_filename[k], argv[nopt]);
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
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
        option_data->bucket_filename = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->bucket_filename);
        strcpy (option_data->bucket_filename, argv[nopt]);
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
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
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
        nopt++; continue;
      }


      /*-----   -fitts filename   -----*/
      if (strcmp(argv[nopt], "-fitts") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need file prefixname after -fitts ");
        option_data->fitts_filename = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->fitts_filename);
        strcpy (option_data->fitts_filename, argv[nopt]);
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
        nopt++;
        continue;
      }


      /*-----   -errts filename   -----*/
      if (strcmp(argv[nopt], "-errts") == 0)
      {
        nopt++;
        if (nopt >= argc)  DC_error ("need file prefixname after -errts ");
        option_data->errts_filename = malloc (sizeof(char)*ALEN(nopt));
        MTEST (option_data->errts_filename);
        strcpy (option_data->errts_filename, argv[nopt]);
        CHECK_NEEDS_FLOATS(argv[nopt]) ;
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

#ifdef PROC_MAX
      if( strcmp(argv[nopt],"-virtvec") == 0 ){  /* 26 Dec 2012 */
        virtu_mrv = 1 ; nopt++ ; continue ;
      }
#endif

#if 0
      /*----- -Dname=val to set environment variable [07 Dec 2007] -----*/

      if( strncmp(argv[nopt],"-D",2) == 0 && strchr(argv[nopt],'=') != NULL ){
        (void) AFNI_setenv( argv[nopt]+2 ) ;
        nopt++ ; continue ;
      }
#endif

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      if( isspace(argv[nopt][0]) ) {
        sprintf(message,"Unknown command line option '%s'\n"
                     "**  Is there a blank after a '\\' character?",argv[nopt]) ;
         DC_error (message);
      } else {
        sprintf(message,"Unrecognized command line option: '%s'\n", argv[nopt]);
        ERROR_message( PROGRAM_NAME " dies: %s",message) ;
        suggest_best_prog_option(argv[0], argv[nopt]);
        exit(1);
      }

    } /***** end of loop over input arguments ****/

   /*----- does user request help menu? -----*/
   if (nopt < 2) {
      sprintf(message,"Too few options");
      DC_error (message);
   }

  /*----- 23 Mar 2007: full first stuff? -----*/

  if( option_data->fout ) option_data->do_fullf = 1 ;

  if( allzero_OK ) use_psinv = 1 ;   /* 30 May 2007 */

  if( option_data->x1D_stop ){       /* 28 Jun 2007 */
    option_data->automask = 0 ;
    option_data->nox1D    = 0 ;
  }

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

  if( option_data->x1D_filename != NULL &&  /* 10 Aug 2010 */
      option_data->nodata               &&
      strncmp(option_data->x1D_filename,"stdout:",7) == 0 ) option_data->x1D_stop = 2 ;

  if( option_data->x1D_filename == NULL && !option_data->nox1D ){
    char *pref=NULL , *cpt ;
    if( option_data->bucket_filename != NULL ){         /* use bucket name? */
      pref = strdup(option_data->bucket_filename) ;
      cpt = strstr(pref,".") ; if( cpt != NULL ) *cpt = '\0' ;
    } else if( option_data->input1D_filename != NULL ){ /* use 1D filename? */
      pref = strdup(option_data->input1D_filename) ;
      cpt = strstr(pref,".1D") ; if( cpt != NULL ) *cpt = '\0' ;
      cpt = strstr(pref,"1D:") ; if( cpt != NULL ) strcpy(pref,"1D") ;
    } else if( option_data->nodata ){                   /* no data? */
      pref = strdup("nodata") ;
    } else {                                            /* default */
      pref = strdup("Decon") ;
    }
    option_data->x1D_filename = malloc(strlen(pref)+32) ;
    strcpy(option_data->x1D_filename,pref) ;
    strcat(option_data->x1D_filename,".xmat.1D") ;
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

  /**** check if we have any data ****/

  if( option_data->num_stimts == 0 && ortar == NULL && option_data->polort == -1 ){
    ERROR_message("'-num_stimts' is 0  AND  no '-ortvec'  AND  '-polort' is -1") ;
    DC_error(     "==> don't you want to DO anything? :-(") ;
  }

  /**** Add -ortvec (ortar) stuff to stimulus file list [02 Dec 2011] ****/

  if( ortar != NULL ){
    int nort=0 , nsold=option_data->num_stimts , nsnew , rr,nn ;
    MRI_IMAGE *oim ;
    for( rr=0 ; rr < IMARR_COUNT(ortar) ; rr++ ) nort += IMARR_SUBIM(ortar,rr)->ny ;
    extend_stim_options( option_data , nort ) ;
    nsnew = option_data->num_stimts ;
    INFO_message("3dDeconvolve extending num_stimts from %d to %d due to -ortvec",nsold,nsnew) ;
    for( nn=nsold,rr=0 ; rr < IMARR_COUNT(ortar) ; rr++ ){
      oim = IMARR_SUBIM(ortar,rr) ;
      for( s=0 ; s < oim->ny ; s++,nn++ ){
        option_data->stim_filename[nn] = (char *)malloc(sizeof(char)*32) ;
        sprintf(option_data->stim_filename[nn],"ortvec:%d,%d",rr,s) ;
        sprintf(option_data->stim_label[nn],"%s[%d]",oim->name,s) ;
        option_data->stim_base[nn] = 1;
      }
    }
  }

  /**** loop and check each stimulus for decent and humane values ****/

  nerr = 0 ; basis_nstim = option_data->num_stimts ; basis_nused = 0 ;
  for( k=0 ; k < option_data->num_stimts ; k++ ){

    if( strncmp(option_data->stim_label[k],"Stim#",5) == 0 )
      WARNING_message("no -stim_label given for stim #%d ==> label = '%s'",
                      k+1 , option_data->stim_label[k] ) ;

    for( s=0 ; s < k ; s++ ){
      if( strcmp(option_data->stim_label[k],option_data->stim_label[s]) == 0 )
        WARNING_message("-stim_label '%s' the same for stim #%d and #%d",
                        option_data->stim_label[k] , s+1 , k+1 ) ;
    }

    if( basis_stim[k] != NULL ){    /* checking -stim_times input */

      basis_stim[k]->name = strdup( option_data->stim_label[k] ) ;

      basis_nused++ ;

      if( option_data->sresp_filename[k] != NULL ) basis_need_mse = 1 ;

      if( option_data->stim_nptr[k] != 1 ){
        ERROR_message(
                "'-stim_nptr %d %d' illegal with '%s'",
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
    for( ic=0 ; ic < num_CENSOR ; ic++ ) /* count number with run != 0 */
      if( abc_CENSOR[ic].i ) nzr++ ;
    if( nzr > 0 && nzr < num_CENSOR )
      WARNING_message(
        "%d -CENSORTR commands have 'run:' numbers and %d do not!\n"
        "   (either all should have 'run:' numbers or none)",nzr,num_CENSOR-nzr);
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
  char *ts_filename,          /* time series file name (plus column index) */
  int *ts_length              /* output value for time series length */
)
{
  char message[THD_MAX_NAME];    /* error message */
  char *cpt;                     /* pointer to column suffix */
  char filename[THD_MAX_NAME];   /* time series file name w/o column index */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE *im, *flim;  /* pointers to image structures
                        -- used to read 1D ASCII */
  float *far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                 /* number of time points in time series */
  int ny;                 /* number of columns in time series file */
  int ipt;                /* time point index */
  float *ts_data = NULL;  /* input time series data */


ENTRY("read_time_series") ;

  /*----- First, check for empty filename -----*/
  if (ts_filename == NULL)
    DC_error ("Missing input time series file name");

  /*** Special case: -ortvec stuff [02 Dec 2011] ***/

  if( ortar != NULL && strncmp(ts_filename,"ortvec:",7) == 0 ){
    int rr=-1 , ss=-1 ;  MRI_IMAGE *oim ; float *oar ;
    sscanf(ts_filename+7,"%d,%d",&rr,&ss) ;
    if( rr < 0 || rr >= IMARR_COUNT(ortar) ) ERROR_exit("3dDeconvolve: %s failure!!!",ts_filename) ;
    oim = IMARR_SUBIM(ortar,rr) ; nx = oim->nx ; ny = oim->ny ;
    if( ss < 0 || ss >= ny ) ERROR_exit("3dDeconvolve: %s failure :-(",ts_filename) ;
    flim = mri_new( nx , 1 , MRI_float ) ;
    far  = MRI_FLOAT_PTR(flim) ;
    oar  = MRI_FLOAT_PTR(oim) + nx*ss ;
    memcpy( far , oar , sizeof(float)*nx ) ;

  } else { /*----- Read the time series file -----*/

    flim = mri_read_1D( ts_filename ) ;
    if (flim == NULL){
      sprintf (message,  "Unable to read time series file: %s",  ts_filename);
      DC_error (message);
    }
  }

  /*-- at this point, data is in the flim struct, however we got it --*/

  far = MRI_FLOAT_PTR(flim);
  nx  = flim->nx;
  ny  = flim->ny;
  if( ny > 1 ){
    if( nx == 1 ){
      MRI_IMAGE *tim = mri_transpose(flim) ;
      mri_free(flim) ; flim = tim ;
      far = MRI_FLOAT_PTR(flim); nx = flim->nx; ny = flim->ny;
      INFO_message("1D time series file %s has %d rows and %d columns: tranposing it",ts_filename,nx,ny);
    } else {
      WARNING_message("1D file %s has %d rows and %d columns: ignoring later columns",ts_filename,nx,ny);
    }
  }

  /*----- Save the time series data -----*/
  *ts_length = nx;
  ts_data = (float *) malloc (sizeof(float) * nx);
  memcpy( ts_data , far , sizeof(float)*nx ) ;
  mri_free(flim) ;
  RETURN (ts_data);
}


/*---------------------------------------------------------------------------*/
/*
  Read the input data files.
*/

void read_input_data
(
  DC_options * option_data,         /* deconvolution program options */
  THD_3dim_dataset ** dset_time,    /* input 3D+time data set */
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
  int nt=0;                /* number of input data time points */
  int nxyz=0;              /* number of voxels */
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
  int nblk,npol , p1,nc , nsl ;
  unsigned int mk,gp ;
  float dtloc=0.0f ;
  float ttmax=big_time ;   /* 28 Aug 2015 */


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
        ERROR_exit("Can't continue after above warnings!");

      for (is = 0;  is < num_stimts;  is++)
      {
        if( basis_stim[is] != NULL ) continue ;  /* 11 Aug 2004: skip this'n */

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
#if 0
      if (num_stimts <= 0)
        DC_error ("Must have num_stimts > 0 for -nodata option");
#endif

      if( option_data->num_slice_base > 0 )
        ERROR_exit("'-nodata' and '-slice_base' are incompatible!") ;

      if( option_data->nodata_TR > 0.0 ) dtloc = option_data->nodata_TR ;

      if( basis_count > 0 ){
             if( option_data->nodata_TR > 0.0 ) basis_TR = option_data->nodata_TR ;
        else if( basis_dtout            > 0.0 ) basis_TR = basis_dtout ;
        else if( option_data->force_TR  > 0.0 ) basis_TR = option_data->force_TR ;
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

      ttmax = basis_TR * nt ; /* 28 Aug 2015 */

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
      else if( option_data->force_TR > 0.0 )
        dtloc = basis_TR = option_data->force_TR;
      if (verb) INFO_message("1D TR is %.3f seconds", basis_TR);

      ttmax = basis_TR * nt ; /* 28 Aug 2015 */
   }

  else if (option_data->input_filename != NULL) /*----- 3D+time dataset -----*/
    {
      int nxd , nyd , nzd , lmax=0 ;

      *dset_time = THD_open_dataset (option_data->input_filename);
      CHECK_OPEN_ERROR(*dset_time,option_data->input_filename);
      if( !option_data->x1D_stop ){
        if( verb ) MEM_MESSAGE ;
        if( verb ) INFO_message("loading dataset %s",option_data->input_filename);
        DSET_load(*dset_time) ; CHECK_LOAD_ERROR(*dset_time) ;
        if( verb ) MEM_MESSAGE ;
      }

      if( option_data->force_TR > 0.0 || (*dset_time)->taxis == NULL ){   /* 18 Aug 2008 */
        float ttt = option_data->force_TR ; if( ttt <= 0.0f ) ttt = 1.0f ;
        EDIT_dset_items( *dset_time ,
                           ADN_ttdel , ttt ,
                           ADN_ntt   , DSET_NVALS(*dset_time) ,
                           ADN_tunits, UNITS_SEC_TYPE ,
                         ADN_none ) ;
        INFO_message("forcibly using TR=%.4f seconds for -input dataset" ,
                     ttt) ;
      }

      nt   = DSET_NVALS(*dset_time); lmax = nt ;
      nxyz = DSET_NVOX (*dset_time);
      nxd  = DSET_NX(*dset_time) ;
      nyd  = DSET_NY(*dset_time) ;
      nzd  = DSET_NZ(*dset_time) ;

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
          WARNING_message("no TR in dataset; setting TR=1 s (?)");
        }
      } else if( basis_TR <= 0.10f ){
        INFO_message("TR in dataset = %g s (less than 0.100 s) -- FYI",
                     basis_TR ) ;  /* one symptom of the PSFB syndrome */
      }

      if( DSET_IS_TCAT(*dset_time) ){  /** 04 Aug 2004: manufacture block list **/
        int lmin=9999 ;
        if( option_data->concat_filename != NULL ){
          WARNING_message(
             "'-concat %s' ignored: input dataset is auto-catenated\n" ,
             option_data->concat_filename ) ;
          option_data->concat_filename = NULL ;
        }
        if( !option_data->tcat_noblock ){
          for( it=0 ; it < (*dset_time)->tcat_num ; it++ ){
            lmin = MIN( lmin , (*dset_time)->tcat_len[it] ) ;
            lmax = MAX( lmax , (*dset_time)->tcat_len[it] ) ;  /* 28 Aug 2015 */
          }
          option_data->tcat_noblock = (lmin < 2) ;
        }
        if( option_data->tcat_noblock ){
          INFO_message("Auto-catenated input datasets treated as one imaging run") ;
          *num_blocks = 1;
          *block_list = (int *) malloc (sizeof(int) * 1);
          (*block_list)[0] = 0;
        } else {
          INFO_message("Auto-catenated input datasets treated as multiple imaging runs") ;
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
      }

      ttmax = dtloc * lmax ; /* 28 Aug 2015 */

      if( option_data->automask ){            /* 15 Apr 2005: automasking */
        MRI_IMAGE *qim ; int mc ;
        qim = THD_rms_brick( *dset_time ) ;
        *mask_vol = mri_automask_image( qim ) ;
        mri_free( qim ) ;
        if( *mask_vol == NULL ){
          WARNING_message("unable to generate automask?!") ;
        } else {
          mc = THD_countmask( nxyz , *mask_vol ) ;
          if( mc <= 1 ){
            WARNING_message("automask is empty!?") ;
            free(*mask_vol) ; *mask_vol = NULL ;
          } else {
            INFO_message("%d voxels in automask (out of %d)", mc,nxyz) ;
          }
        }
        gmask = *mask_vol ;  /* save global mask -- 03 Feb 2009 */
      }

      if (option_data->mask_filename != NULL)   /* read mask from file */
         {
           THD_3dim_dataset *mask_dset = NULL;

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
           gmask = *mask_vol ;         /* global mask save -- 03 Feb 2009 */
         }

      /* 15 Jul 2010: use -STATmask? */

      if( statmask != NULL ){
        if( statmask->nar != nxyz ){
          WARNING_message("-STATmask ignored: doesn't match -input dataset size!") ;
          KILL_bytevec(statmask) ; free(statmask_name) ; statmask_name = NULL ;
        } else {
          int mc ; byte *qmask=gmask ;
          gmask = statmask->ar ; mc = THD_countmask( nxyz , gmask ) ;
          if( mc <= 99 ){
            gmask = qmask ;
            KILL_bytevec(statmask) ; free(statmask_name) ; statmask_name = NULL ;
            WARNING_message("-STATmask ignored: only has %d nonzero voxels",mc) ;
          } else if( verb )
            INFO_message("-STATmask has %d voxels (out of %d = %.1f%%)",
                         mc, nxyz, (100.0f*mc)/nxyz ) ;
        }
      }

      /* 03 Feb 2009 -- make a global mask if not provided thus far */

      if( gmask == NULL && nxd > 15 && nyd > 15 && nzd > 15 ){
        MRI_IMAGE *qim ; int mc ;
        qim   = THD_rms_brick( *dset_time ) ;
        gmask = mri_automask_image( qim ) ;
        mri_free( qim ) ;
        mc = THD_countmask( nxyz , gmask ) ;
        if( mc <= 99 ){ if( gmask != NULL ){ free(gmask) ; gmask = NULL ; } }
        else if( verb && (!floatout || do_FDR) )
          INFO_message("STAT automask has %d voxels (out of %d = %.1f%%)",
                       mc, nxyz, (100.0f*mc)/nxyz ) ;
      }

      EDIT_set_misfit_mask(gmask) ; mri_fdr_setmask(gmask) ;

      /* 23 Dec 2011 -- check for saturation transients */

      if( !dont_do_satcheck ){
        float sum ; int isum ; double qtim = COX_clock_time() ;
        INFO_message("Checking for initial transients") ;
        sum = THD_saturation_check_multi( *dset_time,gmask, *num_blocks,*block_list ) ;
        isum = (int)(sum+0.56789f) ;
        if( isum > 0 ){
          WARNING_message("Dataset seems to have about %d initial transient time points",isum) ;
          if( *num_blocks > 1 )
            WARNING_message(" (This estimate is summed across all imaging runs)" ) ;
        } else {
          ININFO_message("No widespread significant initial transients found") ;
        }
        ININFO_message("Transient check elapsed time = %.2f s",COX_clock_time()-qtim) ;
      } else {
        INFO_message("Skipping check for initial transients") ;
      }

    } else {  /*------------------------- no input data? --------------------*/

      DC_error ("Must specify some sort of input data, or use '-nodata'");

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
          (*block_list)[it] = myfloor (f[it]+0.5);
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
     /* removed special cases of 150->1, 300->2     3 Oct 2007 [rickr] */
     ilen = 1+(int)myfloor(dmax/150.0) ;
     switch( option_data->polort ){
       default:                           /* user supplied non-negative polort */
         if( option_data->polort < ilen )
           WARNING_message(
            "Input polort=%d; Longest run=%.1f s; Recommended minimum polort=%d",
            option_data->polort , dmax , ilen ) ;
         else
           INFO_message(
            "Input polort=%d; Longest run=%.1f s; Recommended minimum polort=%d ++ OK ++",
            option_data->polort , dmax , ilen ) ;
       break ;

       case -1: break ;                   /* user orders no baseline at all */

       case -666:                         /* user orders automatic polort */
         INFO_message("Imaging duration=%.1f s; Automatic polort=%d",dmax,ilen) ;
         option_data->polort = ilen ;
       break ;
     }
   }

  /*-- Create timing for each -stim_times input   [11 Aug 2004] --*/
  /*-- Modified to deal with amplitude modulation [08 Mar 2007] --*/
  /*-- Modified to deal with multiple amplitudes  [03 Dec 2008] --*/

  if( basis_count > 0 ){             /* if there are any, that is */
    MRI_IMAGE *tim , *qim ;
    float     *tar , *qar , *aar[BASIS_MAX_VDIM] , *zar[BASIS_MAX_VDIM] ;
    float toff , tmax,tt,ss , zbar[BASIS_MAX_VDIM] ;
    int   ii,jj , kk , ngood , nx,ny , nf,dd , btyp , nbas ;
    int   nbl=*num_blocks , *bst=*block_list ;
    basis_expansion *be ;
    char *glprefix = "\0" ;  /* 21 May 2008 */
    MRI_IMARR *vimar ; int vdim , vmod , vfun , vv ;

    if( basis_TR    <= 0.0f ) basis_TR    = 1.0f ;
    if( basis_dtout <= 0.0f ) basis_dtout = basis_TR ;

    INFO_message("-stim_times using TR=%g s for stimulus timing conversion",
                 basis_TR) ;
    INFO_message("-stim_times using TR=%g s for any -iresp output datasets",
                 basis_dtout) ;
    if( basis_dtout == basis_TR )
      INFO_message(" [you can alter the -iresp TR via the -TR_times option]");

    if( basis_timetype == GUESS_TIMES ){
      INFO_message(
        "** -stim_times NOTE ** guessing GLOBAL times if 1 time per line; LOCAL otherwise");
      glprefix = "** GUESSED ** " ;
    }

    for( is=0 ; is < num_stimts ; is++ ){
      be = basis_stim[is] ;
      if( be == NULL ) continue ;   /* old style -stim_file: BAH! */

      vimar = NULL ; vdim = vmod = vfun = 0 ;  /* default = no aux params */
      for( vv=0 ; vv < BASIS_MAX_VDIM ; vv++ ){
        aar[vv] = zar[vv] = NULL ; zbar[vv] = 0.0f ;
      }

      btyp = be->type ;  /* 08 Mar 2007: sub-type of -stim_times */

      /** 29 Oct 2007: check TR of -iresp vs. TR of local basis functions **/

      if( strncmp(be->symfun,"TENT"  ,4) == 0 ||
          strncmp(be->symfun,"CSPLIN",6) == 0   ){

        float dx = (be->ttop - be->tbot)/(be->nfunc-1) ;
        if( dx < basis_dtout )
          WARNING_message("%s %d .. %s has inter-knot TR=%g but -iresp output TR=%g",
            be->option , is+1 , be->symfun , dx , basis_dtout ) ;
      }

      /** convert time entries in seconds to global time indexes **/

      tim = basis_times[is] ;        /* image that contains times */
      nx = tim->nx ; ny = tim->ny ;  /* read in from user's file */

      if( tim->kind == MRI_fvect ){  /* modulation */

STATUS("unpacking fvect image") ;

        vimar = mri_fvect_to_imarr(tim) ; /* split them up */
        tar   = MRI_FLOAT_PTR( IMARR_SUBIM(vimar,0) ) ;
        vdim  = IMARR_COUNT(vimar) ;
        for( vv=0 ; vv < vdim-1 ; vv++ )
          aar[vv] = MRI_FLOAT_PTR( IMARR_SUBIM(vimar,vv+1) ) ;

        vmod = be->vmod ; vfun = be->vfun ;

        if( vmod+vfun+1 != vdim )  /* these conditions should not happen */
          ERROR_exit("Error at -stim_times_AM #%d: vmod+vfun+1 != vdim",is+1) ;
        if( vdim != be->vdim )
          ERROR_exit("Error at -stim_times_AM #%d: vdim != be->vdim",is+1) ;

      } else {

        tar = MRI_FLOAT_PTR(tim);  /* just times, no paired value */

      }

      { /** check if all input times are 0s or 1s (a mistake) [15 Aug 2007] **/
        int nzo , nsm ;
        for( nsm=nzo=ii=0 ; ii < nx*ny ; ii++ ){
          if( tar[ii] < big_time ){
            nsm++ ;                               /* number of 'small' values */
            if( tar[ii] == 0.0f || tar[ii] == 1.0f ) nzo++ ; /* number of 0-1 */
          }
        }
        if( nzo > 0 && nzo == nsm )
          WARNING_message("%s %d has all times equal to 0 or 1 ?!?!",
                          be->option , is+1 ) ;
        else if( nsm == 0 )
          WARNING_message("%s %d has all times equal to '*' ?!?!",
                          be->option , is+1 ) ;
      }

      /** 24 Mar 2009: scale all times? **/

      if( stime_fac != 1.0f ){
        int nfac = 0 ;
        for( ii=0 ; ii < nx*ny ; ii++ ){ /* loop over all input times */
          if( tar[ii] > 0.0f && tar[ii] < big_time ){ tar[ii] *= stime_fac ; nfac++ ; }
        }
        if( nfac > 0 )
          INFO_message("Scaled %d times from msec to sec for %s %d" ,
                       nfac , be->option , is+1 ) ;
      }

      /** 24 Mar 2009: subtract from all times? **/

      if( stime_sub != 0.0f ){
        int nsub = 0 , nneg = 0 ;
        for( ii=0 ; ii < nx*ny ; ii++ ){ /* loop over all input times */
          if( tar[ii] < big_time ){
            tar[ii] -= stime_sub ; nsub++ ; if( tar[ii] < 0.0f ) nneg++ ;
          }
        }
        if( nsub > 0 && nneg == 0 )
          INFO_message(
            "Subtracted %.2f sec from %d times for %s %d",
            stime_sub , nsub , be->option , is+1 ) ;
        else if( nsub > 0 && nneg > 0 )
          WARNING_message(
            "Subtracted %.2f sec from %d times for %s %d; %d times < 0 resulted!",
            stime_sub , nsub , be->option , is+1 , nneg ) ;

      }

      if( be->timetype == GUESS_TIMES )
        be->timetype = (nx==1) ? GLOBAL_TIMES : LOCAL_TIMES ;

      ngood = 0 ;                  /* number of good time values found */
      qar   = (float *)calloc(sizeof(float),nx*ny) ; /* collects times */

      for( vv=0 ; vv < vmod+vfun ; vv++ )  /* space to collect amplitudes */
        zar[vv] = (float *)calloc(sizeof(float),nx*ny) ;

      /** convert time in sec (tar) to time in indexes (qar) **/

      if( be->timetype == GLOBAL_TIMES ){  /****------ global times ------****/
        int nbad=0 , nout=0 ; float *psfb=NULL ;

        INFO_message("%s%s %d using GLOBAL times",glprefix,be->option,is+1) ;
        tmax = (nt-1)*basis_TR ;         /* max allowed time offset */
STATUS("loading GLOBAL times and aux params") ;
        for( ii=0 ; ii < nx*ny ; ii++ ){ /* loop over all input times */
          tt = tar[ii] ;
          if( tt >= 0.0f && tt <= tmax ){ /* a good time value */
            for( vv=0 ; vv < vmod+vfun ; vv++ ) zar[vv][ngood] = aar[vv][ii] ;
            qar[ngood++] = tt / basis_TR ;
          } else if( tt >= big_time           ) nbad++ ; /* '*' entries */
            else {                                       /* PSFB entries */
              nout++ ; psfb = (float *)realloc(psfb,sizeof(float)*nout) ;
              psfb[nout-1] = tt ;
            }
        }
        if( nbad )          /* warn about '*' times in GLOBAL input */
          WARNING_message(
           "'%s %d' (GLOBAL) has %d '*' fillers; do you want LOCAL times?",
           be->option , is+1 , nbad ) ;
        if( nout ){         /* warn about times outside the legal range */
          WARNING_message(
           "'%s %d' (GLOBAL) has %d times outside range 0 .. %g [PSFB syndrome]",
           be->option , is+1 , nout , tmax ) ;
           ININFO_message("dataset TR being used is %g s -- unusable times follow",
                          basis_TR) ;
          for( ii=0 ; ii < nout ; ii++ ) fprintf(stderr," %g",psfb[ii]) ;
          fprintf(stderr,"\n") ; free(psfb) ; psfb = NULL ;
        }

        /* check for possibility of 1 stim per run     19 Feb 2014 [rickr] */
        /* i.e. nRUNS>1, ny==nRUNS, nx==1, all times fit in respective run */
        if( ny == *num_blocks && ny > 1 && nx == 1 ){
           int ibot, itop, *bl=*block_list;
           for( it=0; it < ny; it++ ) {
              ibot = bl[it] ;
              itop = (it < ny-1) ? bl[it+1] : nt;
              if( tar[it] > (basis_TR*(itop-ibot-1)) ) break;
           }
           if( it == ny ) /* then might be LOCAL times */
              WARNING_message(
                 "'%s %d' (GLOBAL) has %d runs and %d early events;"
                 " do you want LOCAL times?",
                 be->option, is+1, ny, *num_blocks);
        }

      } else {   /****---------- local times => 1 row per block ----------****/
        int nout ; float *psfb=NULL ;

        INFO_message("%s%s %d using LOCAL times",glprefix,be->option,is+1) ;
        if( ny != nbl ){                 /* times are relative to block */
          WARNING_message(
                  "'%s %d' file '%s' has %d rows,"
                  " but dataset has %d time blocks" ,
                  be->option, is+1, option_data->stim_filename[is], ny,nbl ) ;
          if( ny > nbl ) ny = nbl ;
        }
STATUS("loading LOCAL times and aux params") ;
        for( jj=0 ; jj < ny ; jj++ ){   /* jj=row index=block index */
          if( jj < nbl-1 ) tmax = (bst[jj+1]-1-bst[jj])*basis_TR ;
          else             tmax = (nt       -1-bst[jj])*basis_TR ;
          for( nout=ii=0 ; ii < nx ; ii++ ){
            tt = tar[ii+jj*nx] ;
            if( tt >= 0.0f && tt <= tmax ){
              for( vv=0 ; vv < vmod+vfun ; vv++ ) zar[vv][ngood] = aar[vv][ii+jj*nx] ;
              qar[ngood++] = tt / basis_TR + bst[jj] ;
            } else if( tt < big_time ){         /* PSFB entries */
              nout++ ; psfb = (float *)realloc(psfb,sizeof(float)*nout) ;
              psfb[nout-1] = tt ;
            }
          }
          if( nout ){         /* PSFB strikes again! */
            WARNING_message(
             "'%s %d' (LOCAL) run#%d has %d times outside range 0 .. %g [PSFB syndrome]",
             be->option , is+1 , jj+1 , nout , tmax ) ;
            ININFO_message("dataset TR being used is %g s -- unusable times follow",
                           basis_TR) ;
            for( ii=0 ; ii < nout ; ii++ ) fprintf(stderr," %g",psfb[ii]) ;
            fprintf(stderr,"\n") ; free(psfb) ; psfb = NULL ;
          }
        } /* end of loop over row index */

      } /** end of converting times into indexes **/

STATUS("loading times/params done") ;

      if( ngood > 1 ){  /** check for duplicates [16 Aug 2007] **/
        int ndup=0 , nndup=0 ; float qt , dt ;
        for( ii=0 ; ii < ngood ; ii++ ){
          qt = qar[ii] ;
          for( jj=ii+1 ; jj < ngood ; jj++ ){
            dt = fabsf(qt-qar[jj]) ;
                 if( dt == 0.0f ) ndup++  ;  /* identical */
            else if( dt < 0.50f ) nndup++ ;  /* only 50% of a TR apart */
          }
        }
        if( ndup > 0 || nndup > 0 ){
          WARNING_message(
            "'%s %d' file '%s' has %d duplicate and %d near-duplicate times ???",
            be->option , is+1 , option_data->stim_filename[is] , ndup,nndup ) ;
          if( nndup > 0 )
            ININFO_message(" Where 'near-duplicate' means within 50%% of one TR") ;
          if( be->timetype == GLOBAL_TIMES )
            ININFO_message(" You are using global times: do you want local times?") ;
        }
      }

      /** create qim image to hold time indexes (and paired vals, if present) **/

      if( ngood == 0 ){    /* WTF? no good values found */

        WARNING_message(
                "!! '%s %d' file '%s' has no good stimulus time values\n",
                be->option , is+1 , option_data->stim_filename[is] ) ;
        free((void *)qar) ; qim = NULL ; qar = NULL ;
        for( vv=0 ; vv < vmod+vfun ; vv++ ) free(zar[vv]) ;
        badlev++ ;

      } else if( vmod+vfun == 0 ){  /* no paired values */

        qim = mri_new( ngood,1 , MRI_float ) ;
        qar = (float *)realloc((void *)qar,sizeof(float)*ngood) ;
        memcpy(MRI_FLOAT_PTR(qim),qar,sizeof(float)*ngood) ; /* image of times */

      } else {                   /* 08 Mar 2007: have paired values */

        MRI_IMAGE *yim, *zim ; int nzb , nbad ;
        MRI_IMARR *zimar ;

STATUS("creating new qim") ;

        yim = mri_new_vol_empty(ngood,1,1,MRI_float) ;
        qar = (float *)realloc((void *)qar,sizeof(float)*ngood) ;
        mri_fix_data_pointer( qar , yim ) ;  /* image of times */

        INIT_IMARR(zimar) ; ADDTO_IMARR(zimar,yim) ;
        for( vv=0 ; vv < vmod+vfun ; vv++ ){      /* add images of paired values */
          zim = mri_new_vol_empty(ngood,1,1,MRI_float) ;
          zar[vv] = (float *)realloc((void *)zar[vv],sizeof(float)*ngood) ;
          mri_fix_data_pointer( zar[vv] , zim ) ;  /* image of paired vals */
          ADDTO_IMARR(zimar,zim) ;
        }

        qim = mri_imarr_to_fvect(zimar) ;
        FREE_IMARR(zimar) ;  /* keeps the data, not the MRI_IMAGE junk */

STATUS("checking for bad param values") ;
        for( nzb=nbad=kk=0 ; kk < ngood ; kk++ ){  /* check for bad vals */
          for( jj=vv=0 ; vv < vmod+vfun ; vv++ ){
            if( zar[vv][kk] >= basis_filler ){ jj++; nbad++; zar[vv][kk] = 0.0f; }
            else                             { zbar[vv] += zar[vv][kk] ; }
          }
          if( jj < vmod+vfun ) nzb++ ;
        }
        if( nbad > 0 ){  /* report bad paired vals */
          WARNING_message(
           "!! '%s %d' file '%s': #times=%d #undefined amplitudes=%d",
           be->option, is+1, option_data->stim_filename[is] , ngood,nbad );
          badlev++ ;
        }
        if( nzb > 0 && vmod > 0 ){
          if( AFNI_yesenv("AFNI_3dDeconvolve_rawAM2") ){
            for( vv=0 ; vv < vmod ; vv ++ ) zbar[vv] = 0.0f ;
            INFO_message("'%s %d': not centering amplitude parameters",
                         be->option , is+1 ) ;
          } else {
            for( vv=0 ; vv < vmod ; vv++ ){
              if( be->modsub == NULL || be->modsub[vv] >= basis_filler ){
                zbar[vv] /= nzb ; /* average */
                INFO_message("'%s %d' average amplitude#%d=%g",
                             be->option, is+1,vv+1,zbar[vv]    ) ;
              } else if( btyp == BASIS_MODULATED_PAIR ){
                INFO_message("'%s %d' average amplitude#%d=%g -- but subtracting %g",
                             be->option, is+1,vv+1,zbar[vv],be->modsub[vv] ) ;
                zbar[vv] = be->modsub[vv] ;
              }
            }
          }
        }

      }

      /*** replace input times image with time indexes image created above ***/
      /** (at present, is not ever used again, but you never know, do you?) **/

      mri_free(basis_times[is]) ; basis_times[is] = qim ;

      /*-- create basis vectors for this model now --*/
      /*-- qar[] = array of time indexes
           zar[] = array of amplitudes (if not NULL) --*/

      nf = basis_stim[is]->nfunc ; nbas = basis_stim[is]->nparm ;
      basis_vect[is] = mri_new( nt , nbas , MRI_float ) ;  /* all zeros */

      if( qar != NULL ){
        int imin,imax , ibot,itop ;
        float *bv = MRI_FLOAT_PTR(basis_vect[is]) ;
        float dbot,dtop , fnt=(float)nt , z1 , z2[BASIS_MAX_VDIM] , eps ;

#if 0
fprintf(stderr,"%s %d: adjusted time indexes follow:\n",be->option,is+1) ;
for( kk=0 ; kk < ngood ; kk++ ) fprintf(stderr," %g",qar[kk]) ;
fprintf(stderr,"\n") ;
#endif

        dbot = be->tbot / basis_TR ; /* range of indexes about each stim time */
        dtop = be->ttop / basis_TR ;
        imin = 0 ; imax = nt-1 ;     /* for the case of nbl=1 */
        eps  = 0.001f * basis_TR ;

        for( kk=0 ; kk < ngood ; kk++ ){   /* for the kk-th stim time */
          tt = qar[kk] ; if( tt < 0.0f || tt >= fnt ) continue ;

          z1 = 1.0f ;      /* default amplitude for first function set */
          if( vmod > 0 ){  /* compute modulated amplitude(s) */
            if( btyp == BASIS_MODULATED_MONO ) z1 = 0.0f ;
            for( vv=0 ; vv < vmod ; vv++ ){
              switch( btyp ){
                case BASIS_MODULATED_MONO: z1    += zar[vv][kk]         ; break;
                case BASIS_MODULATED_PAIR: z2[vv] = zar[vv][kk]-zbar[vv]; break;
              }
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

          ibot = (int)myceil ( tt + dbot ) ; if( ibot < imin ) ibot = imin ;
          itop = (int)myfloor( tt + dtop ) ; if( itop > imax ) itop = imax ;

#if 0
INFO_message("stim timedex=%g dbot=%g dtop=%g ibot=%d itop=%d",tt,dbot,dtop,ibot,itop) ;
#endif

          if( vfun > 0 ){ /* set vfun params in basis functions [08 Dec 2008] */
            for( jj=0 ; jj < nf ; jj++ ){
              for( vv=0 ; vv < vfun ; vv++ )
                basis_func_setparn( be->bfunc[jj] , vv , zar[vv+vmod][kk] ) ;
            }
          }

          for( ii=ibot ; ii <= itop ; ii++ ){   /* loop over active interval */
            ss = basis_TR*(ii-tt) ;                         /* shifted time */
#if 0
ININFO_message("ss=%g tbot=%g ttop=%g ss-ttop=%g",ss,be->tbot,be->ttop,ss-be->ttop) ;
#endif
            if( ss+eps < be->tbot || ss-eps > be->ttop ) continue ; /* nugatory */
            if( z1 != 0.0f ){
              for( jj=0 ; jj < nf ; jj++ )
                bv[ii+jj*nt] +=
                  z1 * basis_funceval( be->bfunc[jj], basis_TR*(ii-tt) );
            }

            if( vmod > 0 && btyp == BASIS_MODULATED_PAIR ){
              for( vv=0 ; vv < vmod ; vv++ )
                for( jj=0 ; jj < nf ; jj++ )
                  bv[ii+(jj+(vv+1)*nf)*nt] +=
                    z2[vv] * basis_funceval( be->bfunc[jj], basis_TR*(ii-tt) );
            }
          }
          if( btyp == BASIS_MODULATED_INDV ) bv += nf*nt ; /* 16 Jul 2007 */
        } /* end of loop over stimulus times */
#if 0
fprintf(stderr,"-stim_times %d: basis vectors follow:\n",is+1) ;
bv = MRI_FLOAT_PTR(basis_vect[is]) ;
for( ii=0 ; ii < nt ; ii++ ){
  fprintf(stderr,"%d:",ii) ;
  for( jj=0 ; jj < nbas ; jj++ ) fprintf(stderr," %g",bv[ii+jj*nt]) ;
  fprintf(stderr,"\n") ;
}
#endif

        free(qar) ;
        for( vv=0 ; vv < vmod+vfun ; vv++ ) free(zar[vv]) ;
        DESTROY_IMARR(vimar) ;

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
  ncoldat = p ;        /* 16 Aug 2019 */
  nblk = *num_blocks ;
  npol = option_data->polort ;

  p1 = 0 ;
  for( is=0 ; is < nblk ; is++ ){
    for( it=0 ; it <= npol ; it++ ){
      cd.mask  = CM_BASELINE_MASK | CM_POLORT_MASK ;
      cd.group = -1 ;                            /* polort group = -1 */
      sprintf(cd.name,"Run%c%dPol%c%d",index_prefix,is+1,index_prefix,it) ;
      option_data->coldat[p1++] = cd ;
    }
  }

  STIMLABEL_stuff        = (stimlabel_stuff *)malloc(sizeof(stimlabel_stuff)) ;
  STIMLABEL_stuff->cbot  = (int *)            malloc(sizeof(int)*num_stimts) ;
  STIMLABEL_stuff->ctop  = (int *)            malloc(sizeof(int)*num_stimts) ;
  STIMLABEL_stuff->label = (char **)          malloc(sizeof(char *)*num_stimts);

  for( nsl=is=0 ; is < num_stimts ; is++ ){
    if( basis_stim[is] != NULL ){
      nc = basis_stim[is]->nparm ;
      mk = 0 ; gp = is+1 ;
    } else {
      nc = max_lag[is] - min_lag[is] + 1 ;
      if( baseline[is] ){ mk = CM_BASELINE_MASK; gp = 0   ; }
      else              { mk = 0               ; gp = is+1; }
    }
    if( gp > 0 ){
      STIMLABEL_stuff->cbot[nsl]  = p1 ;
      STIMLABEL_stuff->ctop [nsl] = p1+nc-1 ;
      STIMLABEL_stuff->label[nsl] = option_data->stim_label[is] ; nsl++ ;
    }
    for( it=0 ; it < nc ; it++ ){
      cd.mask  = mk ; cd.group = gp ;
      sprintf(cd.name,"%-1.60s%c%d",option_data->stim_label[is],index_prefix,it) ;
      DEBLANK(cd.name) ;
      option_data->coldat[p1++] = cd ;
    }
  }
  coldat = option_data->coldat ;  /* global variable */
  STIMLABEL_stuff->nstim = nsl ;

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
  THD_3dim_dataset * dset_time,     /* input 3D+time data set */
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
  if( ierror > 0 && !THD_ok_overwrite() ){   /* ZSS: Dec. 16 08 */
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
  THD_3dim_dataset * dset_time    /* input 3D+time data set */
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
  THD_3dim_dataset * dset_time,   /* input 3D+time data set */
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
  int nt;                  /* number of images in input 3D+time dataset */
  int NFirst;              /* first image from input 3D+time dataset to use */
  int NLast;               /* last image from input 3D+time dataset to use */
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
      sprintf (message, "Input censor time series file %s is too short (%d < %d)",
             option_data->censor_filename,censor_length,nt);
      DC_error (message);
    }
  else if( censor_length > nt ){  /* 19 Jul 2013 */
    WARNING_message("Input censor time series file %s is too long (%d > %d)",
             option_data->censor_filename,censor_length,nt);
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
    if( NFirst > 0 )  /* 04 Oct 2007: warn the user about this 'feature' */
      INFO_message(
       "First time point used in analysis = index #%d (from max maxlag)",NFirst) ;
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

  if( N < nt )  /* 03 Nov 2010 [Tea Party Day] */
    INFO_message("Number of time points: %d (before censor) ; %d (after)",nt,N) ;
  else
    INFO_message("Number of time points: %d (no censoring)",nt) ;
  ININFO_message("Number of parameters:  %d [%d baseline ; %d signal]",p,q,p-q) ;

  /*----- Check for sufficient data -----*/
  if (N == 0)  DC_error ("No usable time points? :(");
  if (N <= p)
    {
       if( nt > p )  /* Better grieving when death happens [13 Feb 2018] */
         ERROR_message(" *** Censoring has made regression impossible :( ***") ;
       else
         ERROR_message("Regression model has too many parameters for dataset length :(") ;
       sprintf (message,  "Insufficient data (%d) for estimating %d parameters", N,p);
       DC_error (message);
   }
  option_data->N = N;

  /* save some things in global variables for later reference */

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
      } else if( stim_length[is] > nt*nptr[is] ){ /* 02 Jul 2009 */

        WARNING_message(
          "-stim_file %d: file length is %d, longer than expected %d (from dataset)",
          is+1 , stim_length[is] , nt*nptr[is] ) ;
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
            sprintf (message, "Only %d time points for 3D+time dataset %s\n",
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

static long long zvf_totalbytes = 0 ;  /* 04 Mar 2008 */

void zero_fill_volume (float ** fvol, int nxyz)
{
  int ixyz;

  zvf_totalbytes += (long long) (sizeof(float) * nxyz) ;  /* 04 Mar 2008 */

  if( proc_numjob == 1 ){ /* 1 process ==> allocate locally */

    *fvol  = (float *) malloc (sizeof(float) * nxyz);

    if( *fvol == NULL ){  /* 04 Mar 2008 */
      MEM_MESSAGE ;
      ERROR_message("Memory allocation for output sub-bricks fails!") ;
      ERROR_message("Have allocated %s bytes (about %s) for output, up to now",
                    commaized_integer_string(zvf_totalbytes) ,
                    approximate_number_string((double)zvf_totalbytes) ) ;
      ERROR_message("Potential lenitives or palliatives:\n"
                    " ++ Use 3dZcutup to cut input dataset into\n"
                    "      smaller volumes, then 3dZcat to put\n"
                    "      the results datasets back together.\n"
                    " ++ Reduce the number of output sub-bricks.\n"
                    " ++ Use a system with more memory and/or swap space."
                   ) ;
      ERROR_exit("Alas, 3dDeconvolve cannot continue under these circumstances.") ;
    }

    for (ixyz = 0;  ixyz < nxyz;  ixyz++) (*fvol)[ixyz]  = 0.0;

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
     "Total shared memory needed = %s >= %s (2 GB)\n"
     "** SUGGESTION 1:  Use 3dAutobox to automatically eliminate non-brain\n"
     "   areas from the 3d+time input dataset and reduce memory \n"
     "   requirements,  e.g.\n"
     "     3dAutobox -prefix Subj1AllRuns_Smaller -input Subj1AllRuns\n"
     "   Then run 3dDeconvolve again with the smaller 3d+time input dataset\n"
     "\n"
     "** SUGGESTION 2:  Use 3dZcutup to slice dataset into smaller pieces\n"
     "**                and then 3dZcat to glue results back together.\n"
     "\n"
     "** SUGGESTION 3:  Run on a 64-bit computer system, instead of 32-bit.\n"
    , commaized_integer_string(psum) , commaized_integer_string(twogig) ) ;
   else {
     INFO_message("total shared memory needed = %s bytes (about %s)" ,
                  commaized_integer_string(psum) ,
                  approximate_number_string((double)psum) ) ;
     if( verb ) MEM_MESSAGE ;
   }

   proc_shmsize = psum ;  /* global variable */

   /*------- create shared memory segment -------*/

#ifdef MAP_ANON  /** 24 Oct 2005: use mmap() instead of shmem **/

#undef MY_MMAP_FLAGS
#ifdef MAP_NORESERVE  /* Solaris */
# define MY_MMAP_FLAGS (MAP_ANON | MAP_SHARED | MAP_NORESERVE)
#else
# define MY_MMAP_FLAGS (MAP_ANON | MAP_SHARED)
#endif

   proc_shmptr = mmap( (void *)0 , (size_t)psum ,
                       PROT_READ | PROT_WRITE , MY_MMAP_FLAGS ,
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
     fprintf(stderr,"\n** Can't create shared memory of size %s!\n"
                      "** Try re-running without -jobs option!\n" ,
             commaized_integer_string(proc_shmsize) ) ;

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
               "** https://afni.nimh.nih.gov/afni/doc/misc/afni_parallelize\n"
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
   INFO_message("mmap() memory allocated: %s bytes (about %s)\n" ,
                commaized_integer_string(proc_shmsize),
                approximate_number_string((double)proc_shmsize) );
#else
   INFO_message("Shared memory allocated: %s bytes at id=%d\n" ,
                commaized_integer_string(proc_shmsize) , proc_shmid ) ;
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
  int nt;                  /* number of images in input 3D+time dataset */
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
  THD_3dim_dataset ** dset_time,    /* input 3D+time data set */
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

   set_obliquity_report(0); /* silence obliquity */

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
  if (!(*option_data)->nodata){
    allocate_memory (*option_data, coef_vol, scoef_vol, tcoef_vol,
                 fpart_vol, rpart_vol, mse_vol, ffull_vol, rfull_vol,
                 glt_coef_vol, glt_tcoef_vol, glt_fstat_vol, glt_rstat_vol,
                 fitts_vol, errts_vol);

    INFO_message("Memory required for output bricks = %s bytes (about %s)",
                 commaized_integer_string(zvf_totalbytes) ,
                 approximate_number_string((double)zvf_totalbytes) ) ;
  }

  EXRETURN ;
}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3D+time data set.
*/

void extract_ts_array
(
  THD_3dim_dataset * dset_time,      /* input 3D+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
)

{
  MRI_IMAGE * im;          /* intermediate float data */
  float * ar;              /* pointer to float data */
  int ts_length;           /* length of input 3D+time data set */
  int it;                  /* time index */


  /*----- Extract time series from 3D+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  DC_error ("Unable to extract data from 3D+time dataset");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ts_length = DSET_NVALS(dset_time);
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
  int nt,                      /* number of images in input 3D+time dataset */
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
  int ilag=0;              /* time lag index */
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
  THD_3dim_dataset * dset,          /* input 3D+time data set */
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
#else
# define do_get 0
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

  int nt;                  /* number of images in input 3D+time dataset */
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
  FILE *mfp=NULL ;             /* 26 Dec 2012 */

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

  if( num_glt > 0 ){
    GLT_stuff          = (glt_stuff *)malloc(sizeof(glt_stuff)) ;
    GLT_stuff->glt_num = num_glt ;
    GLT_stuff->glt_lab = glt_label ;
    GLT_stuff->glt_mat = glt_cmat ;
  }

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

  /*-- save the matrix in various ways, depending on the user's whimsy --*/

  if (option_data->xout)  matrix_sprint ("X matrix:", xdata);

  if( option_data->xjpeg_filename != NULL )    /* 21 Jul 2004 - a picture */
    JPEG_matrix_gray( xdata , option_data->xjpeg_filename ) ;

  if( option_data->x1D_filename   != NULL ){   /* 28 Mar 2006 - a file */
    void *cd=(void *)coldat ; int *gl=good_list ;
    if( AFNI_noenv("AFNI_3dDeconvolve_NIML") &&
        strstr(option_data->x1D_filename,"niml") == NULL ) cd = NULL ;
    ONED_matrix_save( xdata , option_data->x1D_filename , cd , N,gl ,
                      (is_xfull) ? &xfull : NULL , num_blocks,block_list ,
                      (void *)GLT_stuff , (void *)STIMLABEL_stuff ) ;

    /*-- 22 Aug 2008: 3dREMLfit notice, and other announcements --*/

    if( !option_data->nodata && cd != NULL && verb ){
      char *iname=NULL ;  /* input filename for command echo below */
      char *cname=NULL ;  /* command to be output for user's wisdom */
      char *pref , *cpt ;
      FILE *fp ;
      int iadd=0 , ilen , oneline=AFNI_yesenv("AFNI_3dDeconvolve_oneline") ;
      char *lbreak ;
      char *mod_prefix; /* non-AFNI?, for surface datasets  14 Nov 2018 [rickr] */

      if( option_data->input_filename != NULL ){
        iname = calloc( sizeof(char) , strlen(option_data->input_filename)+9 ) ;
        if( THD_filename_ok(option_data->input_filename) ){
          strcpy(iname,option_data->input_filename) ;
        } else {
          strcpy(iname,"\"") ;
          strcat(iname,option_data->input_filename) ; strcat(iname,"\"") ;
        }
      }
      cname = THD_zzprintf( cname ,
                            "3dREMLfit -matrix %s", option_data->x1D_filename );
      lbreak = (oneline) ? "" : " \\\n" ;
      if( iname != NULL ){
        if( *iname != '"' )
          cname = THD_zzprintf( cname , " -input %s%s" , iname,lbreak ) ;
        else
          cname = THD_zzprintf( cname , "%s -input %s%s" , lbreak,iname,lbreak ) ;
        free(iname) ;
      } else if( !oneline ){
          cname = THD_zzprintf( cname , "%s" , lbreak ) ;
      }
      if( option_data->mask_filename ){
        ilen  = strlen(cname) ;
        cname = THD_zzprintf( cname , " -mask %s" , option_data->mask_filename );
        iadd += strlen(cname)-ilen ;
      } else if( option_data->automask ){
        ilen  = strlen(cname) ;
        cname = THD_zzprintf( cname , " -automask" );
        iadd += strlen(cname)-ilen ;
      }
      if( CoefFilename != NULL ){
        if( iadd > 50 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
        ilen  = strlen(cname) ;
        cname = THD_zzprintf( cname ,
                              " -Rbeta %s_REML" , CoefFilename ) ;
        iadd += strlen(cname)-ilen ;
      }
      if( option_data->bucket_filename != NULL ){
        /* handle non-AFNI dataset formats, like niml.dset for surface data */
        mod_prefix = option_data->bucket_filename; /* init to current dset */
        if( has_known_non_afni_extension(mod_prefix) )
           mod_prefix = without_afni_filename_extension(mod_prefix); /* no free */

        if( iadd > 50 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
        ilen  = strlen(cname) ;
        if( option_data->fout ) cname = THD_zzprintf( cname , " -fout") ;
        if( option_data->tout ) cname = THD_zzprintf( cname , " -tout") ;
        if( option_data->rout ) cname = THD_zzprintf( cname , " -rout") ;
        iadd += strlen(cname)-ilen ; ilen = strlen(cname) ;
        if( iadd > 40 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
        cname = THD_zzprintf( cname , " -Rbuck %s_REML" ,  mod_prefix );
        cname = THD_zzprintf( cname , " -Rvar %s_REMLvar", mod_prefix );
        iadd += strlen(cname)-ilen ;
      }
      if( option_data->fitts_filename != NULL ){
        mod_prefix = option_data->fitts_filename; /* handle non-AFNI formats */
        if( has_known_non_afni_extension(mod_prefix) )
           mod_prefix = without_afni_filename_extension(mod_prefix); /* no free */

        if( iadd > 50 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
        ilen  = strlen(cname) ;
        cname = THD_zzprintf( cname , " -Rfitts %s_REML", mod_prefix );
        iadd += strlen(cname)-ilen ;
      }
      if( option_data->errts_filename != NULL ){
        mod_prefix = option_data->errts_filename; /* handle non-AFNI formats */
        if( has_known_non_afni_extension(mod_prefix) )
           mod_prefix = without_afni_filename_extension(mod_prefix); /* no free */

        if( iadd > 50 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
        ilen  = strlen(cname) ;
        cname = THD_zzprintf( cname , " -Rerrts %s_REML", mod_prefix );
        iadd += strlen(cname)-ilen ;
      }
#if 0
      if( iadd > 70 && !oneline ){ cname = THD_zzprintf(cname," \\\n"); iadd=0; }
#endif
      ilen  = strlen(cname) ;
      cname = THD_zzprintf( cname , " -verb" ) ;
      iadd += strlen(cname)-ilen ;

      INFO_message("========= Things you can do with the matrix file =========");
      INFO_message(
        "(a) Linear regression with ARMA(1,1) modeling of serial correlation:\n\n"
        "%s\n " , cname ) ;

      if( option_data->bucket_filename != NULL ){         /* use bucket name? */
        pref = strdup(option_data->bucket_filename) ;
        cpt = strstr(pref,".") ; if( cpt != NULL ) *cpt = '\0' ;
      } else if( option_data->input1D_filename != NULL ){ /* use 1D filename? */
        pref = strdup(option_data->input1D_filename) ;
        cpt = strstr(pref,".1D") ; if( cpt != NULL ) *cpt = '\0' ;
        cpt = strstr(pref,"1D:") ; if( cpt != NULL ) strcpy(pref,"1D") ;
      } else if( option_data->nodata ){                   /* no data? */
        pref = strdup("nodata") ;
      } else {                                            /* default */
        pref = strdup("3dDeconvolve") ;
      }
      pref = (char *)realloc(pref,strlen(pref)+16) ; strcat(pref,".REML_cmd") ;
      fp = fopen(pref,"w") ;
      if( fp != NULL ){
        fprintf( fp, "# %s\n", (commandline!=NULL) ? commandline : PROGRAM_NAME );
        /* allow options (should apply to bash or tcsh)  23 Apr 2010 [rickr] */
        fprintf( fp, "\n%s $*\n", cname ) ;
        fclose(fp) ;
        INFO_message("N.B.: 3dREMLfit command above written to file %s",pref) ;
      }
      free(pref) ; free(cname) ;

      INFO_message("(b) Visualization/analysis of the matrix via ExamineXmat.R");
      INFO_message("(c) Synthesis of sub-model datasets using 3dSynthesize") ;
      INFO_message("==========================================================");
    }
  }

  if( is_xfull && option_data->x1D_unc != NULL ){   /* 25 Mar 2007 - full matrix? */
    void *cd=(void *)coldat ; int *gl=good_list ;
    if( AFNI_noenv("AFNI_3dDeconvolve_NIML") &&
        strstr(option_data->x1D_filename,"niml") == NULL ) cd = NULL ;
    ONED_matrix_save( xfull , option_data->x1D_unc , cd , xfull.rows,NULL , &xfull,
                      num_blocks,block_list , NULL, (void *)STIMLABEL_stuff ) ;
  }

  if( is_xfull_plus && option_data->x1D_regcen != NULL ){ /* 16 Aug 2019 - full+ matrix? */
    void *cd=(void *)coldat ; int *gl=good_list ;
    if( AFNI_noenv("AFNI_3dDeconvolve_NIML") &&
        strstr(option_data->x1D_filename,"niml") == NULL ) cd = NULL ;
    ONED_matrix_save( xfull_plus , option_data->x1D_regcen , cd , xfull_plus.rows,NULL , &xfull_plus,
                      num_blocks,block_list , NULL, (void *)STIMLABEL_stuff ) ;
  }

  /*----- 14 Jul 2004: check matrix for bad columns - RWCox -----*/

  { int *iar , k , nerr=0 ;
    iar = matrix_check_columns( xdata , QEPS ) ;
    if( iar != NULL ){
      WARNING_message("-------------------------------------------------") ;
      WARNING_message("Problems with the X matrix columns, listed below:") ;
      for( k=0 ; iar[2*k] >= 0 ; k++ ){
        if( iar[2*k+1] >= 0 ){
          WARNING_message(
             "!! * Columns %d [%s] and %d [%s] are (nearly?) collinear!",
                  iar[2*k]  ,COLUMN_LABEL(iar[2*k]  ),
                  iar[2*k+1],COLUMN_LABEL(iar[2*k+1]) ) ;
          nerr++ ; badlev++ ;
        } else {
          char *ww = (allzero_OK) ? ("  ") : ("!!") ;
          WARNING_message("%s * Column %d [%s] is all zeros",
                  ww , iar[2*k] , COLUMN_LABEL(iar[2*k]) ) ;
          if( !allzero_OK ) badlev++ ;
        }
      }
      WARNING_message("-------------------------------------------------") ;
      if( nerr > 0 && AFNI_yesenv("AFNI_3dDeconvolve_nodup") )
        ERROR_exit("Can't continue after above problems/warnings!");
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
      qbad = check_matrix_condition( xext, "stim_base-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }

    /* extract columns for -polort model only */

    for( nlist=jj=0 ; jj < xdata.cols ; jj++ )
      if( coldat[jj].group < 0 ) clist[nlist++] = jj ;
    if( nlist > 0 ){
      matrix_initialize (&xext);
      matrix_extract( xdata , nlist , clist , &xext ) ;
      qbad = check_matrix_condition( xext, "polort-only" ); badlev += qbad;
      matrix_destroy( &xext ) ;
    }
  }

  /** 22 Jul 2010: move the exit to AFTER condition number reports **/

  if( option_data->x1D_stop ){   /* 28 Jun 2007 -- my work here is done */
    if( option_data->x1D_stop == 1 )
      INFO_message("3dDeconvolve exits: -x1D_stop option was invoked") ;
    exit(0) ;
  }

  /*----- Initialization for the regression analysis -----*/

  init_regression_analysis (p, qp, num_stimts, baseline, min_lag, max_lag,
                      xdata, &x_full, &xtxinv_full, &xtxinvxt_full,
                      &x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);
  if( option_data->xout )
    matrix_sprint ("(X'X) inverse matrix:", xtxinv_full);

  if( nodata && option_data->x1D_filename != NULL &&
      strncmp(option_data->x1D_filename,"stdout:",7) != 0 ){
    char fn[THD_MAX_NAME] , *jpt ;
    strcpy(fn,option_data->x1D_filename) ;
                       jpt = strstr(fn,".xmat") ;
    if( jpt == NULL )  jpt = strstr(fn,".1D") ;
    if( jpt == NULL )  jpt = fn + strlen(fn) ;
    strcpy(jpt,"_XtXinv.xmat.1D") ;
    ONED_matrix_save( xtxinv_full,fn,
                      NULL,0,NULL,NULL,0,NULL,NULL,NULL ) ; /* no column metadata */
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

STATUS("storing Xcol_inbase") ;
    for( is=0 ; is < qp ; is++ ) Xcol_inbase[is] = 1 ; /* mark baseline columns */
    m = qp ;
    for( is=0 ; is < num_stimts ; is++ ){
      npar = (basis_stim[is] != NULL)
            ? basis_stim[is]->nparm
            : option_data->stim_maxlag[is] - option_data->stim_minlag[is] + 1 ;

      if( baseline[is] ) for( j=0 ; j < npar ; j++ ) Xcol_inbase[m+j] = 1 ;
      m += npar ;
    }

STATUS("computing Xcol_mean") ;
    mmax = 0.0f ;
    for( j=0 ; j < p ; j++ ){   /* compute mean of each column */
      sum = 0.0 ;
      for( i=0 ; i < X.rows ; i++ ) sum += X.elts[i][j] ;
      Xcol_mean[j] = (float)(sum/X.rows) ;
      if( Xcol_inbase[j] && fabs(Xcol_mean[j]) > mmax )  /* find largest */
        mmax = fabs(Xcol_mean[j]) ;             /* mean of baseline cols */
    }

    if( mmax > 0.0f ){    /* mark baseline cols that have nontrivial means */
STATUS("re-marking Xcol_inbase") ;
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
STATUS("computing [xtxinvxt_full] [xdata] - I") ;
    for( ii=0 ; ii < mm ; ii++ ){
      for( jj=0 ; jj < mm ; jj++ ){
        sum = (ii==jj) ? -1.0 : 0.0 ;
        for( kk=0 ; kk < nn ; kk++ )
          sum += xtxinvxt_full.elts[ii][kk]*xdata.elts[kk][jj] ;
        esum += fabs(sum) ;
      }
    }
STATUS("computing message from esum") ;
    esum /= (mm*mm) ;
         if( esum > 1.e-3 || !IS_GOOD_FLOAT(esum) )
                           { www = " ** BEWARE **"   ; badlev++; }
    else if( esum > 1.e-4 ){ www = " ++ OK ++"       ; }
    else if( esum > 1.e-6 ){ www = " ++ GOOD ++"     ; }
    else                   { www = " ++ VERY GOOD ++"; }
    if( strstr(www,"**") != NULL )
      WARNING_message("+++++ !! Matrix inverse average error = %g %s",esum,www) ;
    else
      INFO_message("+++++ Matrix inverse average error = %g %s",esum,www) ;
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
      if( jpt == NULL ){ jpt = strstr(fn,".png") ; jsuf = ".png" ; }
      if( jpt == NULL ){ jpt = strstr(fn,".PNG") ; jsuf = ".PNG" ; }
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
      ONED_matrix_save( xpsinv , fn ,
                        NULL,0,NULL,NULL,0,NULL,NULL,NULL ) ; /* no column metadata */
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
            "!! " PROGRAM_NAME ": Can't run past %d matrix warnings without '-GOFORIT %d'",
            badlev , badlev ) ;
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

          proc_numjob = 1 ; virtu_mrv = 0 ;

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

          if( virtu_mrv ){  /* create a vectim and virtualize it, for Javier */
            INFO_message("-virtvec: Starting creation of virtual vector image") ;
            MEM_MESSAGE ;
            inset_mrv = THD_dset_to_vectim( dset , mask_vol , 0 ) ;
            if( inset_mrv == NULL ){
              ERROR_message("Can't create vector image in RAM?!") ; virtu_mrv = 0 ;
            } else {
              DSET_unload(dset) ;
              INFO_message("vector image created in RAM") ;
              MEM_MESSAGE ;
              fname_mrv = mri_get_tempfilename("JUNK") ;
              pp = THD_vectim_data_tofile( inset_mrv , fname_mrv ) ;
              if( pp == 0 ){
                ERROR_message("Can't write vector image to temp file %s",fname_mrv) ;
                virtu_mrv = 0 ; free(fname_mrv) ; VECTIM_destroy(inset_mrv) ;
                DSET_load(dset) ; MEM_MESSAGE ;
              } else {
                free(inset_mrv->fvec) ; inset_mrv->fvec = NULL ;
                INFO_message("vector image stored in temp file %s",fname_mrv) ;
                MEM_MESSAGE ;
#ifdef USE_GET
                do_get = 0 ;
#endif
              }
            }
          }

          /* start processes */

          if( !option_data->quiet ) INFO_message("Voxels in dataset: %d",nxyz);
          if( nvox < nxyz )
          if( !option_data->quiet ) INFO_message("Voxels in mask:    %d",nvox);
          if( !option_data->quiet ) INFO_message("Voxels per job:    %d",nper);

          for( pp=1 ; pp < proc_numjob ; pp++ ){
            ixyz_bot = proc_vox_bot[pp] ;   /* these 3 variables   */
            ixyz_top = proc_vox_top[pp] ;   /* are for the process */
            proc_ind = pp ;                 /* we're about to fork */
            errno    = 0 ;
            newpid   = fork() ;
            if( newpid == -1 ){
              ERROR_message("Can't fork job #%d! Danger, Wil Robinson!",pp);
              if( errno != 0 ) perror("** Unix ERROR message") ;
              if( pp > 1 ){
                int qq ;
                for( qq=1 ; qq < pp ; qq++ ){
                  ERROR_message("Killing fork-ed job %d (pid=%u)",
                                qq , (unsigned int)proc_pid[qq]   ) ;
                  kill(    proc_pid[qq] ,SIGTERM   ) ; iochan_sleep(10) ;
                  waitpid( proc_pid[qq] , NULL , 0 ) ;
                }
              }
              ERROR_exit("3dDeconvolve main process now stopping -- SORRY") ;
            }

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
      } else if ( virtu_mrv ) { /* only if virtu_mrv  17 Jan 2013 [rickr] */

        WARNING_message("-virtvec: ignoring option since -jobs isn't greater than 1") ;
        virtu_mrv = 0 ;  /* 26 Dec 2012 */

      }
#endif /* PROC_MAX */

      if( proc_numjob == 1 && !option_data->quiet ){
        MEM_MESSAGE ;
        INFO_message("Calculations starting; elapsed time=%.3f",COX_clock_time()) ;
      }

      /* show voxel loop when numjob > 1        17 Sep 2007 [rickr] */
      vstep = (ixyz_top - ixyz_bot) / 50 ;
      if( option_data->quiet        ||
          option_data->fdisp >= 0.0 ||
          option_data->progress > 0 ||
          (proc_numjob > 1 && proc_ind != 0) ) vstep = 0 ;

      if( vstep > 0 ) fprintf(stderr,"++ voxel loop:") ;

      if( virtu_mrv && fname_mrv != NULL ){  /* 26 Dec 2012 */
        mfp = fopen(fname_mrv,"r") ;
        if( mfp == NULL ) ERROR_exit("Job #%d: can't re-open temp file %s",proc_ind,fname_mrv) ;
        if( ts_array == NULL ){
          ts_array = (float *) malloc (sizeof(float) * nt);   MTEST (ts_array);
        }
      }

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
            if( do_get ){
              ts_array = MRI_FLOAT_PTR(IMARR_SUBIM(imget,cget));  /* the GET way */
              cget++ ;  /* take this one next time */
            } else if( mfp != NULL ){                             /* Javier's way */
              int ijk = THD_vectim_ifind( ixyz , inset_mrv ) ;
              if( ijk >= 0 ) THD_vector_fromfile( nt , ijk , ts_array , mfp ) ;
              else           memset( ts_array , 0 , sizeof(float)*nt ) ;
            } else {
              extract_ts_array (dset, ixyz, ts_array);           /* the OLD way */
            }
#else
            extract_ts_array (dset, ixyz, ts_array);             /* the OLD way */
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

        if( mfp != NULL ) fclose(mfp) ;  /* 26 Dec 2012 */

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

          if( inset_mrv != NULL ){
            VECTIM_destroy(inset_mrv) ; remove(fname_mrv) ;
            INFO_message("-virtvec: temp file %s has been removed",fname_mrv) ;
            free(fname_mrv) ; fname_mrv = NULL ; virtu_mrv = 0 ;
          }
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

  if( option_data->force_TR > 0.0 )   /* 18 Aug 2008 */
    EDIT_dset_items( dset ,
                       ADN_ttdel , option_data->force_TR ,
                       ADN_ntt   , DSET_NVALS(dset) ,
                       ADN_tunits, UNITS_SEC_TYPE ,
                     ADN_none ) ;

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
  Routine to write one AFNI 3D+time data set.
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

  if( option_data->force_TR > 0.0 )   /* 18 Aug 2008 */
    EDIT_dset_items( dset ,
                       ADN_ttdel , option_data->force_TR ,
                       ADN_ntt   , DSET_NVALS(dset) ,
                       ADN_tunits, UNITS_SEC_TYPE ,
                     ADN_none ) ;

  nxyz = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
  DSET_UNMSEC(dset) ;  /* 12 Aug 2005 */
  newtr = DSET_TIMESTEP(dset) / nptr;

  /*----- allocate memory -----*/

  dtype = (floatout) ? MRI_float : MRI_short ;

  /*-- make an empty copy of the prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (dset);

  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;

  sprintf (label, "Output prefix: %s", output_filename);
  if( commandline != NULL )
     tross_multi_Append_History( new_dset , commandline,label,NULL ) ;
  else
     tross_Append_History ( new_dset, label);

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
  if(!THD_ok_overwrite() &&          /* ZSS: Dec. 16 08 */
      THD_deconflict_prefix(new_dset) > 0 ) {
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename,DSET_PREFIX(new_dset) ) ;
  }
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

          EDIT_misfit_report( DSET_FILECODE(new_dset) , ib ,
                              nxyz , factor , bar , volume ) ;
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

    EDIT_misfit_report( DSET_FILECODE(new_dset) , ibrick ,
                        nxyz , factor , sbr , volume ) ;
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
  int nt;                   /* number of images in input 3D+time dataset */
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

  if( option_data->force_TR > 0.0 )   /* 18 Aug 2008 */
    EDIT_dset_items( old_dset ,
                       ADN_ttdel , option_data->force_TR ,
                       ADN_ntt   , DSET_NVALS(old_dset) ,
                       ADN_tunits, UNITS_SEC_TYPE ,
                     ADN_none ) ;

  DSET_UNMSEC(old_dset) ;  /* 12 Aug 2005 */

  bout = !option_data->nobout ;
  cout = !option_data->nocout ;

  /*----- Initialize local variables -----*/
  nxyz = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;
  num_stimts = option_data->num_stimts;
  nt = DSET_NVALS(old_dset);
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
  if( strcmp(output_prefix,"Decon") != 0 &&
      !THD_ok_overwrite() &&                   /* ZSS: Dec. 16 08 */
      THD_deconflict_prefix(new_dset) > 0 ) {
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_prefix,DSET_PREFIX(new_dset) ) ;
   }
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
     if( !THD_ok_overwrite() &&                   /* ZSS: Dec. 16 08 */
          THD_deconflict_prefix(coef_dset) > 0 )   {
       WARNING_message("Filename conflict: changing '%s' to '%s'",
                       CoefFilename,DSET_PREFIX(coef_dset) ) ;
     }
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
        sprintf (brick_label, "%s_GLT%c%d_Coef", label, EDIT_get_index_prefix() , ilc);
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
            sprintf (brick_label, "%s_GLT%c%d_Tstat", label, EDIT_get_index_prefix() , ilc);
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

  if( do_FDR && !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
    ibrick = THD_create_all_fdrcurves( new_dset ) ;
  else
    ibrick = 0 ;

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  if (! option_data->quiet){
    INFO_message("Wrote bucket dataset into %s",  DSET_BRIKNAME(new_dset));
    if( ibrick > 0 )
      ININFO_message("created %d FDR curves in bucket header",ibrick) ;
  }


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
  int nt;                   /* number of images in input 3D+time dataset */
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


  /*----- Write the impulse response function 3D+time dataset -----*/
  ib = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      if( basis_stim[is] != NULL ){                    /* until later */
        if( option_data->iresp_filename[is] != NULL ) {
          if( option_data->input_filename )
              basis_write_iresp( argc , argv , option_data ,
                                 basis_stim[is] , basis_dtout ,
                                 coef_vol+ib    ,
                                 option_data->iresp_filename[is] ) ;
          else if( option_data->input1D_filename )
              basis_write_iresp_1D( argc , argv , option_data ,
                                 basis_stim[is] , basis_dtout ,
                                 coef_vol+ib    ,
                                 option_data->iresp_filename[is] ) ;
        }
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


  /*----- Write the standard deviation 3D+time dataset -----*/
  ib = qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      if( basis_stim[is] != NULL ){                     /* until later */
        if( option_data->sresp_filename[is] != NULL ) {
          if( option_data->input_filename )
             basis_write_sresp( argc , argv , option_data ,
                                basis_stim[is] , basis_dtout ,
                                mse_vol , ib , XtXinv ,
                                option_data->sresp_filename[is] ) ;
          /* 19 Aug 2011 [rickr] */
          else if( option_data->input1D_filename )
             basis_write_sresp_1D( argc , argv , option_data ,
                                basis_stim[is] , basis_dtout ,
                                mse_vol , ib , XtXinv ,
                                option_data->sresp_filename[is] ) ;
        }
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


  /*----- Write the fitted (full model) 3D+time dataset -----*/
  if (option_data->fitts_filename != NULL)
    if (nxyz > 1)
      write_ts_array (argc, argv, option_data, nt, 1, 0, fitts_vol,
                  option_data->fitts_filename);
    else
      write_one_ts (option_data->fitts_filename, nt, fitts_vol);



  /*----- Write the residual errors 3D+time dataset -----*/
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
  int nt;                   /* number of images in input 3D+time dataset */


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
  THD_3dim_dataset * dset_time = NULL;    /* input 3D+time data set */
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
  mainENTRY(PROGRAM_NAME " main") ; AFNI_process_environ(NULL) ; machdep() ;
  SET_message_file( PROGRAM_NAME ".err" ) ;

  index_prefix = EDIT_get_index_prefix() ;

  commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;

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
#if 0
  if (mask_vol != NULL){ free (mask_vol); mask_vol = NULL; }
#endif


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
   int nts=X.cols , npt=X.rows , ii,jj , nxim=2*768 , nyim=2*1024 ;
   MEM_plotdata *mp ;
   float **xar ;
   MRI_IMAGE *im ;
   char *eee ;

   if( nts < 1 || npt < 2 ) return NULL ;

   xar = (float **)malloc( sizeof(float *)*nts ) ;
   if( !xar ) {
     ERROR_message("PLOT_mg: failed to alloc %d float ptrs", nts);
     return NULL;
   }
   for( jj=0 ; jj < nts ; jj++ ){
     xar[jj] = (float *)malloc( sizeof(float)*npt ) ;
     if( !xar[jj] ) {
       ERROR_message("PLOT_mg: failed to alloc %d floats @jj=%d", npt, jj);
       return NULL;
     }
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

#undef  PLOT_MAX
#define PLOT_MAX 3333

void JPEG_matrix_gray( matrix X , char *fname )
{
   MRI_IMAGE *im ;

   if( fname == NULL || *fname == '\0' ) return ;

   if( X.cols > PLOT_MAX || X.rows > PLOT_MAX ){  /* 30 Apr 2015 */
     WARNING_message("Can't plot %s -- matrix size %dx%d exceeds max=%d",
                     fname , X.rows , X.cols , PLOT_MAX ) ;
     return ;
   }

   im = PLOT_matrix_gray( X ) ;
   if( im == NULL ){
     WARNING_message("Can't plot %s -- internal error!",fname) ;
     return ;
   }

   mri_write_jpg( fname , im ) ;

   if( verb ) INFO_message("Wrote matrix image to file %s",fname) ;
   mri_free(im) ; return ;
}

/*----------------------------------------------------------------------------*/
/*! Save matrix to a .1D text file */

void ONED_matrix_save( matrix X , char *fname , void *xd , int Ngl, int *gl,
                       matrix *Xff , int nbl, int *bl , void *gs, void *ss )
{
   int nx=X.rows , ny=X.cols , ii,jj,kk ;
   column_metadata *cd = (column_metadata *)xd ;
   matrix Xf ; int nxf=0,nyf ;
   glt_stuff       *gst = (glt_stuff *)      gs ;
   stimlabel_stuff *sst = (stimlabel_stuff *)ss ;

   if( fname == NULL || *fname == '\0' ) return ;

   if( Xff != NULL ){
     Xf = *Xff ; nxf = Xf.rows ; nyf = Xf.cols ;
     if( nyf != ny ){
       WARNING_message("X matrix save: ncol(X)=%d but ncol(Xf)=%d ?",ny,nyf) ;
       nxf = 0 ;
     }
   }

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
       strcpy( lll , (jj < ncoldat) ? cd[jj].name : "cens" ) ;
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
     NI_set_attribute( nel,"ColumnMasks",lab ); free((void *)lab); lab = NULL;
#endif
#if 1
     { NI_int_array iar ;
       iar.num = ny ; iar.ar = (int *)malloc(sizeof(int)*ny) ;
       for( jj=0 ; jj < ny ; jj++ ) iar.ar[jj] = cd[jj].group ;
       lab = NI_encode_int_list( &iar , "," ) ;
       NI_set_attribute( nel,"ColumnGroups",lab ); NI_free((void *)lab); lab = NULL;
     }
#endif
#if 1
     lab = THD_zzprintf( lab , "%g" , basis_TR ) ;
     NI_set_attribute( nel, "RowTR", lab ); free((void *)lab); lab = NULL;
#endif
#if 1
     if( Ngl > 0 ){
       NI_int_array iar ;
       iar.num = Ngl ;
       if( gl != NULL ) iar.ar = gl ;
       else {
         iar.ar = (int *)malloc(sizeof(int)*Ngl) ;
         for( jj=0 ; jj < Ngl ; jj++ ) iar.ar[jj] = jj ;
       }
       lab = NI_encode_int_list( &iar , "," ) ;
       NI_set_attribute( nel, "GoodList", lab ); NI_free((void *)lab); lab = NULL;
       if( iar.ar != gl ) free((void *)iar.ar) ;
     }
     if( nxf > 0 ){
       sprintf(lll,"%d",nxf) ;
       NI_set_attribute( nel , "NRowFull" , lll ) ;
     }
#endif
#if 1
     if( nbl > 0 && bl != NULL ){  /* Bastille Day, 2008 */
       NI_int_array iar ;
       iar.num = nbl ;
       iar.ar  = bl ;
       lab = NI_encode_int_list( &iar , "," ) ;
       NI_set_attribute( nel, "RunStart", lab ); NI_free((void *)lab); lab = NULL;
     }
#endif
#if 1
     if( sst != NULL && sst->nstim > 0 ){   /* 31 Jul 2008 */
       NI_int_array iar ;
       sprintf(lll,"%d",sst->nstim) ;
       NI_set_attribute( nel , "Nstim" , lll ) ;
       iar.num = sst->nstim ;
       iar.ar  = sst->cbot ;
       lab = NI_encode_int_list( &iar , "," ) ;
       NI_set_attribute( nel, "StimBots", lab );NI_free((void *)lab); lab = NULL;
       iar.ar  = sst->ctop ;
       lab = NI_encode_int_list( &iar , "," ) ;
       NI_set_attribute( nel, "StimTops", lab );NI_free((void *)lab); lab = NULL;
       for( jj=0 ; jj < sst->nstim ; jj++ ){
         strcpy(lll,sst->label[jj]) ;
         for( ii=0 ; lll[ii] != '\0' ; ii++ ) if( isspace(lll[ii]) ) lll[ii]='_';
         if( jj < sst->nstim-1 ) strcat(lll," ; ") ;
         lab = THD_zzprintf( lab , "%s" , lll ) ;
       }
       NI_set_attribute( nel, "StimLabels", lab ); free((void *)lab); lab = NULL;
     }
#endif
#if 1
     if( gst != NULL && gst->glt_num > 0 ){  /* 31 Jul 2008 */
       matrix gm ; NI_float_array far ; int nn,mm ;
       sprintf(lll,"%d",gst->glt_num) ;
       NI_set_attribute( nel , "Nglt" , lll ) ;
       for( kk=0 ; kk < gst->glt_num ; kk++ ){
         strcpy(lll,gst->glt_lab[kk]) ;
         for( ii=0 ; lll[ii] != '\0' ; ii++ ) if( isspace(lll[ii]) ) lll[ii]='_';
         if( kk < gst->glt_num-1 ) strcat(lll," ; ") ;
         lab = THD_zzprintf( lab , "%s" , lll ) ;
       }
       NI_set_attribute( nel, "GltLabels", lab ); free((void *)lab); lab = NULL;
       for( kk=0 ; kk < gst->glt_num ; kk++ ){
         gm = gst->glt_mat[kk] ;
         nn = gm.rows ;
         mm = gm.cols ;
         far.num   = 2 + nn*mm ;
         far.ar    = (float *)malloc(sizeof(float)*far.num) ;
         far.ar[0] = (float)nn ;
         far.ar[1] = (float)mm ;
         for( ii=0 ; ii < nn ; ii++ )
           for( jj=0 ; jj < mm ; jj++ )
             far.ar[jj+2+ii*mm] = (float)gm.elts[ii][jj] ;
         lab = NI_encode_float_list( &far , "," ) ;
         sprintf(lll,"GltMatrix_%06d",kk) ;
         NI_set_attribute( nel, lll, lab ); NI_free((void *)lab); lab = NULL;
         free((void *)far.ar) ;
       }
     }
#endif
#if 1
    if( basis_nused > 0 ){
      char qbuf[128] ;
      sprintf(lll,"%d",basis_nstim) ;
      NI_set_attribute( nel, "BasisNstim", lll ) ;
      for( kk=0 ; kk < basis_nstim ; kk++ ){
        if( basis_stim[kk] == NULL ) continue ;
        sprintf(lll,"BasisOption_%06d",kk+1) ;
        NI_set_attribute( nel, lll, basis_stim[kk]->option ) ;
        sprintf(lll,"BasisName_%06d",kk+1) ;
        NI_set_attribute( nel, lll, basis_stim[kk]->name ) ;
        sprintf(lll,"BasisFormula_%06d",kk+1) ;
        NI_set_attribute( nel, lll, basis_stim[kk]->symfun ) ;
        sprintf(lll,"BasisColumns_%06d",kk+1) ;
        sprintf(qbuf,"%d:%d",basis_stim[kk]->pbot,basis_stim[kk]->pbot+basis_stim[kk]->nparm-1) ;
        NI_set_attribute( nel , lll, qbuf ) ;
      }
    }
#endif
#if 1
     if( commandline != NULL ){
       NI_set_attribute( nel , "CommandLine" , commandline ) ;
     }
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
/* SEE NI_write_element_tostring() */
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
   float *cdar=NULL , ssef=0.0f , *volume ;
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
       sprintf (brick_label, "%s_GLT%c%d_Coef", glt_label[iglt], EDIT_get_index_prefix(), ilc);
#endif
       volume = glt_coef_vol[iglt][ilc];
       attach_sub_brick( dset_buck, ivol, volume, nxyz,
                         FUNC_FIM_TYPE, brick_label, 0, 0, 0, NULL);
       free((void *)volume) ; ivol++ ;

       if( tout ){
#ifdef USE_OLD_LABELS
         sprintf( brick_label , "%s LC[%d] t-st" , glt_label[iglt], ilc ) ;
#else
         sprintf (brick_label, "%s_GLT%c%d_Tstat", glt_label[iglt], EDIT_get_index_prefix(),ilc);
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

   putenv("AFNI_DECONFLICT=OVERWRITE") ;        /* overwrite output dataset */

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
         if( fvv == NULL || fvv->nvec < 1 ) {
            /* skip any empty row, else fail      17 Oct 2013 [rickr] */
            if( fpt ) WARNING_message("skipping empty SYM row at posn %d of %s",
                                      (int)(fpt-fdup)+4, fname);
            else ERROR_exit("failing on empty SYM");
            buf = fpt+1;
            continue ;
         }
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
         cpt = afni_fgets( buf , 8192 , fp ) ; /* read next line */
         if( cpt == NULL ) break ;             /* end of input? */
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
       printf("------------------------------------------------------------\n");
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
   float val ;
        if( x <= bot || x >= top ) val = 0.0f ;
   else if( x <= mid )             val = (x-bot)/(mid-bot) ;
   else                            val = (top-x)/(top-mid) ;
#if 0
ININFO_message("basis_tent(x=%g,bot=%g,mid=%g,top=%g)=%g",x,bot,mid,top,val) ;
#endif
   return val ;
}

/*--------------------------------------------------------------------------*/
#undef  CA
#define CA  0.5f   /* negative of slope at x=1 */
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
   float y=(x-a)/dx , bot=-2.0f , top=2.0f , val ; int gg=(int)flag ;
   switch(gg){
     case -2: bot =  0.0f ; break ;  /* at left edge */
     case -1: bot = -1.0f ; break ;  /* 1 in from left edge */
     case  1: top =  1.0f ; break ;  /* 1 in from right edge */
     case  2: top =  0.0f ; break ;  /* at right edge */
   }
   bot -= 0.0009f ; top += 0.0009f ;
   if( y < bot || y > top ) val = 0.0f ;
   else                     val = hh_csplin(y) ;
#if 0
ININFO_message("basis_csplin(x=%g,a=%g,dx=%g,flag=%d)=%g  [bot=%g top=%g y=%g]",
               x,a,dx,gg,val , bot,top,y ) ;
#endif
   return val ;
}

/*--------------------------------------------------------------------------*/
/* Basis function that is 1 inside the bot..top interval, 0 outside of it.
----------------------------------------------------------------------------*/

static float basis_one( float x, float bot, float top, float junk, void *q )
{
   float eps=0.0009f ;
   if( x < bot-eps || x > top+eps ) return 0.0f ;
   return 1.0f ;
}

/*--------------------------------------------------------------------------*/
/* Cosine basis function:
    - 0 for x outside bot..top range
    - cos(freq*(x-bot)) otherwise.
----------------------------------------------------------------------------*/

static float basis_cos( float x, float bot, float top, float freq, void *q )
{
   float eps=0.0009f ;
   if( x < bot-eps || x > top+eps ) return 0.0f ;
   return (float)cosf(freq*(x-bot)) ;
}

/*--------------------------------------------------------------------------*/
/* Sine basis function:
    - 0 for x outside bot..top range
    - sin(freq*(x-bot)) otherwise.
----------------------------------------------------------------------------*/

static float basis_sin( float x, float bot, float top, float freq, void *q )
{
   if( x <= bot || x >= top ) return 0.0f ;
   return (float)sinf(freq*(x-bot)) ;
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

/*---------- TWOGAM function; c is ignored [06 Jan 2018] ----------*/

static float basis_twogam( float x , float b , float c , float top , void *q )
{
   float *fq = (float *)q ;
   float p1,p2 , rr , q1,q2 , g1,g2 ;
   if( x <= 0.0f || x > top || q == NULL ) return 0.0f ;
   p1 = fq[0] ; q1=fq[1] ; rr = fq[2] ; p2 = fq[3] ; q2 = fq[4] ;
   g1 = basis_gam( x , p1,q1 , top , NULL ) ;
   g2 = basis_gam( x , p2,q2 , top , NULL ) ;
   return b*(g1-rr*g2) ;  /* b is scale factor */
}

/*---------- convert peak and width to p and q for GAM [06 Jan 2018] ----------*/

static float_pair gam_peak_fwhm_convert( float peak , float fwhm )
{
   float_pair result = {0.0f,0.0f} ;
   double_pair pq ;
   float p , q ;

   if( peak <= 0.0f || fwhm <= 0.0f ) return result ;

#if 1
   pq = gam_find_pq( (double)peak , (double)fwhm ) ; /* cs_gamfit.c */
   p = pq.a ; q = pq.b ;
#else
   p = (2.35f*peak/fwhm) ; p = p*p ; q = peak/p ;
#endif

   INFO_message("GAM conversion: peak=%g fwhm=%g -> p=%g q=%g",peak,fwhm,p,q) ;
   if( p <= 0.0f || q <= 0.0f )
     ERROR_exit("GAM conversion from peak+fwhm to p,q parameters fails :(") ;

   result.a = p ; result.b = q ; return result ;
}

/*--------------------------------------------------------------------------*/
/* MION basis function [12 Jul 2010] */

static float basis_mion( float x, float b, float c, float top, void *q )
{
   if( x <= 0.0f || x > 60.0f ) return 0.0f ;

   return 16.4486f * ( -0.184f/ 1.5f * expf(-x/ 1.5f)
                       +0.330f/ 4.5f * expf(-x/ 4.5f)
                       +0.670f/13.5f * expf(-x/13.5f) ) ;
}

/*--------------------------------------------------------------------------*/
/* MIONN basis function [02 Apr 2012] */

static float basis_mionn( float x, float b, float c, float top, void *q )
{
   if( x <= 0.0f || x > 60.0f ) return 0.0f ;

   return -16.4486f * ( -0.184f/ 1.5f * expf(-x/ 1.5f)
                        +0.330f/ 4.5f * expf(-x/ 4.5f)
                        +0.670f/13.5f * expf(-x/13.5f) ) ;
}

/*--------------------------------------------------------------------------*/
/* SPMG basis functions (corrected 29 May 2007, I hope) */

#undef  SPM_A1
#undef  SPM_A2
#undef  SPM_P1
#undef  SPM_P2

#define SPM_A1 0.0083333333    /* A * exp(-x) * x^P */
#define SPM_P1 5.0             /* not 4.0 -- 29 May 2007 ! */
#define SPM_A2 1.274527e-13
#define SPM_P2 15.0

static float basis_spmg1( float x, float a, float b, float c, void *q )
{
   if( x <= 0.0f || x >= 25.0f ) return 0.0f ;
   return (float)(exp(-x)*( SPM_A1*pow(x,SPM_P1)
                           -SPM_A2*pow(x,SPM_P2) )) ;
}

/*--------------------------- d/dx of the above ----------------------------*/

static float basis_spmg2( float x, float a, float b, float c, void *q )
{
   if( x <= 0.0f || x >= 25.0f ) return 0.0f ;
   return (float)(exp(-x)*( SPM_A1*pow(x,SPM_P1-1.0)*(SPM_P1-x)
                           -SPM_A2*pow(x,SPM_P2-1.0)*(SPM_P2-x) )) ;
}

/*--------------------------------------------------------------------------*/

#undef  SPM_A3
#undef  SPM_P3
#define SPM_A3 0.00869011   /* SPMG3 added 27 Jul 2007 -- per Gang Chen */
#define SPM_P3 4.94057

static float basis_spmg3( float x, float a, float b, float c, void *q )
{
   float d0 , cc ;
   if( x <= 0.0f || x >= 25.0f ) return 0.0f ;
   d0 =   SPM_A3 * pow(x,SPM_P3) * exp(-x/1.01)
        - SPM_A2 * pow(x,SPM_P2) * exp(-x)     ;
   cc = basis_spmg1( x,a,b,c,q ) ;
   return (100.0f*(cc-d0)) ;
}

/*--------------------------------------------------------------------------*/

static float waveform_SPMG1( float t ){ return basis_spmg1(t,0.0f,0.0f,0.0f,NULL); }
static float waveform_SPMG2( float t ){ return basis_spmg2(t,0.0f,0.0f,0.0f,NULL); }
static float waveform_SPMG3( float t ){ return basis_spmg3(t,0.0f,0.0f,0.0f,NULL); }
static float waveform_MION ( float t ){ return basis_mion (t,0.0f,0.0f,0.0f,NULL); }
static float waveform_MIONN( float t ){ return basis_mionn(t,0.0f,0.0f,0.0f,NULL); }

static float GAM_p , GAM_q , GAM_top ;
static float waveform_GAM( float t ){ return basis_gam(t,GAM_p,GAM_q,GAM_top,NULL); }

static float TWOGAM_parm[6] , TWOGAM_top ;
static float waveform_TWOGAM( float t )
{
  return basis_twogam( t , 1.0f , 0.0f , TWOGAM_top , TWOGAM_parm ) ;
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

/*----------------------------------------------------------------------------*/
/* for BLOCK4, dmBLOCK4, and dmUBLOCK4:
    t    = time of evaluation
    T    = duration
    peak =      if( peak >  0 ) ==> arrange so peak amplitude of curve is 'peak'
           else if( peak == 0 ) ==> let amplitude come from duration, and
                                      amplitude(duration=big) = 1
           else if( peak <  0 ) ==> let amplitude come from duration, with
                                      amplitude(duration=-peak) = 1
*//*--------------------------------------------------------------------------*/

static float basis_block4_NEW( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp , TT ;

   w = basis_block_hrf4(t,T) ; /* function value, but need to alter amplitude */

   if( w > 0.0f ){
          if( peak >  0.0f ){ TT = T ; }
     else if( peak == 0.0f ){ TT = 99.9f ; peak = 1.0f ; }
     else                   { TT = -peak ; peak = 1.0f ; }
     tp = TPEAK4(TT) ; pp = basis_block_hrf4(tp,TT) ;
     if( pp > 0.0f ) w *= peak / pp ;
   }

   return w ;
}

static float basis_block4_OLD( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp ;

   w = basis_block_hrf4(t,T) ;

   if( w > 0.0f && peak > 0.0f ){
     tp = TPEAK4(T) ; pp = basis_block_hrf4(tp,T) ;
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

/*--------------------------------------------------------------------------*/


static float basis_block5_NEW( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp , TT ;

   w = basis_block_hrf5(t,T) ;

   if( w > 0.0f ){
          if( peak >  0.0f ){ TT = T ; }
     else if( peak == 0.0f ){ TT = 99.9f ; peak = 1.0f ; }
     else                   { TT = -peak ; peak = 1.0f ; }
     tp = TPEAK5(TT) ; pp = basis_block_hrf5(tp,TT) ;
     if( pp > 0.0f ) w *= peak / pp ;
   }

   return w ;
}

static float basis_block5_OLD( float t, float T, float peak, float junk, void *q )
{
   float w , tp , pp ;

   w = basis_block_hrf5(t,T) ;

   if( w > 0.0f && peak > 0.0f ){
     tp = TPEAK5(T) ; pp = basis_block_hrf5(tp,T) ;
     if( pp > 0.0f ) w *= peak / pp ;
   }

   return w ;
}

/*--------------------------------------------------------------------------*/
/* Legendre polynomial basis function
    - 0 for x outside range bot..top
    - P_n(x), x scaled to be -1..1 over range bot..top (for integer n >= 0)
----------------------------------------------------------------------------*/

static float basis_legendre( float x, float bot, float top, float n, void *q )
{
   float xq ; int m ;

   x = 2.0f*(x-bot)/(top-bot) - 1.0f ;  /* now in range -1..1 */

   if( x < -1.000001f || x > 1.000001f ) return 0.0f ;

   xq = x*x ; m = (int)n ; if( m < 0 ) return 0.0f ;

   switch( m ){
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

    /** orders above 9 added 14 Aug 2007 **/

    case 10:
      return -0.24609375e0 + (0.1353515625e2 + (-0.1173046875e3 +
              (0.3519140625e3 + (-0.42732421875e3 + 0.18042578125e3 * xq)
             * xq) * xq) * xq) * xq;

    case 11:
      return (-0.270703125e1 + (0.5865234375e2 + (-0.3519140625e3 +
             (0.8546484375e3 + (-0.90212890625e3 + 0.34444921875e3 * xq)
             * xq) * xq) * xq) * xq) * x;

    case 12:
      return 0.2255859375e0 + (-0.17595703125e2 + (0.2199462890625e3 +
             (-0.99708984375e3 + (0.20297900390625e4 + (-0.1894470703125e4
             + 0.6601943359375e3 * xq) * xq) * xq) * xq) * xq)
             * xq;

    case 13:
      return (0.29326171875e1 + (-0.87978515625e2 + (0.7478173828125e3 +
             (-0.270638671875e4 + (0.47361767578125e4 + (-0.3961166015625e4
             + 0.12696044921875e4 * xq) * xq) * xq) * xq) * xq)
            * xq) * x;

    case 14:
      return -0.20947265625e0 + (0.2199462890625e2 + (-0.37390869140625e3 +
             (0.2368088378906250e4 + (-0.710426513671875e4 +
             (0.1089320654296875e5 + (-0.825242919921875e4 +
            0.244852294921875e4 * xq) * xq) * xq) * xq) * xq)
           * xq) * xq;

    case 15:
      return (-0.314208984375e1 + (0.12463623046875e3 + (-0.142085302734375e4
            + (0.7104265136718750e4 + (-0.1815534423828125e5 +
              (0.2475728759765625e5 + (-0.1713966064453125e5 +
               0.473381103515625e4 * xq) * xq) * xq) * xq)
             * xq) * xq) * xq) * x;

    case 16:
      return 0.196380615234375e0 + (-0.26707763671875e2 + (0.5920220947265625e3
            + (-0.4972985595703125e4 + (0.2042476226806641e5 +
              (-0.4538836059570312e5 + (0.5570389709472656e5 +
              (-0.3550358276367188e5 + 0.9171758880615234e4 * xq) * xq)
            * xq) * xq) * xq) * xq) * xq) * xq;

    case 17:
      return (0.3338470458984375e1 + (-0.1691491699218750e3 +
             (0.2486492797851562e4 + (-0.1633980981445312e5 +
             (0.5673545074462891e5 + (-0.1114077941894531e6 +
             (0.1242625396728516e6 + (-0.7337407104492188e5 +
              0.1780400253295898e5 * xq) * xq) * xq) * xq)
           * xq) * xq) * xq) * xq) * x;

    case 18:
      return -0.1854705810546875e0 + (0.3171546936035156e2 +
            (-0.8880331420898438e3 + (0.9531555725097656e4 +
            (-0.5106190567016602e5 + (0.1531857170104980e6 +
            (-0.2692355026245117e6 + (0.2751527664184570e6 +
            (-0.1513340215301514e6 + 0.3461889381408691e5 * xq) * xq)
           * xq) * xq) * xq) * xq) * xq) * xq) * xq;

    case 19:
      return (-0.3523941040039062e1 + (0.2220082855224609e3 +
             (-0.4084952453613281e4 + (0.3404127044677734e5 +
             (-0.1531857170104980e6 + (0.4038532539367676e6 +
             (-0.6420231216430664e6 + (0.6053360861206055e6 +
             (-0.3115700443267822e6 + 0.6741574058532715e5 * xq) * xq)
          * xq) * xq) * xq) * xq) * xq) * xq) * xq) * x;

    case 20:
      return 0.1761970520019531e0 + (-0.3700138092041016e2 +
            (0.1276547641754150e4 + (-0.1702063522338867e5 +
            (0.1148892877578735e6 + (-0.4442385793304443e6 +
            (0.1043287572669983e7 + (-0.1513340215301514e7 +
            (0.1324172688388824e7 + (-0.6404495355606079e6 +
             0.1314606941413879e6 * xq) * xq) * xq) * xq) * xq)
            * xq) * xq) * xq) * xq) * xq;

   } /* end of switch on m */

   /** if here, m > 20 ==> use recurrence relation **/

   { float pk=0, pkm1, pkm2 ; int k ;
     pkm2 = basis_legendre( x , -1.0f , 1.0f , 19.0f , NULL ) ;
     pkm1 = basis_legendre( x , -1.0f , 1.0f , 20.0f , NULL ) ;
     for( k=21 ; k <= m ; k++ , pkm2=pkm1 , pkm1=pk )
       pk = ((2.0f*k-1.0f)*x*pkm1 - (k-1.0f)*pkm2)/k ;
     return pk ;
   }
}

#undef  POLY_MAX
#define POLY_MAX 20  /* max order allowed in function above */

/*--------------------------------------------------------------------------*/
#define ITT 19  /* index for symbol 't' */
#define IXX 23  /* 'x' */
#define IZZ 25  /* 'z' */
/*------------------------------------------------------*/
/*! Basis function given by a user-supplied expression. */

static float basis_expr( float x, float bot, float top, float dtinv, void *q )
{
   PARSER_code *pc = (PARSER_code *)q ;
   double atoz[26] , val ; float eps=0.0009f ;

   if( x < bot-eps || x > top+eps ) return 0.0f ;
   memset(atoz,0,sizeof(double)*26) ;         /* set to 0 [24 Mar 2009] */
   atoz[ITT] = x ;                            /* t = true time from stim */
   atoz[IXX] = (x-bot)*dtinv ;                /* x = scaled to [0,1] */
   atoz[IZZ] = 2.0*atoz[IXX] - 1.0 ;          /* z = scaled to [-1,1] */
   val = PARSER_evaluate_one( pc , atoz ) ;
   return (float)val ;
}

/*==========================================================================*/
/**---------- Implementation of the Cox WAV function from waver.c ---------**/
/*==========================================================================*/

/*----------------------------------------------------------------*/

static float WAV_rise_start, WAV_fall_start ,
             WAV_fall_end  , WAV_restore_end ;

static float WAV_delay_time   =  2.0f ,
             WAV_rise_time    =  4.0f ,
             WAV_fall_time    =  6.0f ,
             WAV_undershoot   =  0.2f ,
             WAV_restore_time =  2.0f  ;

/*----------------------------------------------------------------*/

static void setup_WAV_constants(void)
{
   WAV_rise_start  = WAV_delay_time ;
   WAV_fall_start  = WAV_rise_start + WAV_rise_time ;
   WAV_fall_end    = WAV_fall_start + WAV_fall_time ;
   WAV_restore_end = WAV_fall_end   + WAV_restore_time ;
   return ;
}

/*----------------------------------------------------------------
  Function that transitions from 0 to 1 over input x in [0,1].
------------------------------------------------------------------*/

#define ZT_FAC 0.50212657f
#define ZT_ADD 0.99576486f

static INLINE float ztone( float x )
{
   register double y ;

   if( x <= 0.0f ) return 0.0f ; else if( x >= 1.0f ) return 1.0f ;

   y = (0.5*PI) * ( 1.6 * x - 0.8 ) ;
   return (float)(ZT_FAC * ( tanh(tan(y)) + ZT_ADD )) ;
}

/*----------------------------------------------------------------*/
/*! Basic WAV waveform at time lag t */

static float waveform_WAV( float t )
{
   if( t < WAV_rise_start )
      return 0.0f ;

   if( t < WAV_fall_start )
      return ztone( (t-WAV_rise_start)/WAV_rise_time ) ;

   if( t < WAV_fall_end )
      return (1.0f+WAV_undershoot) * ztone( (WAV_fall_end-t)/WAV_fall_time )
             - WAV_undershoot ;

   if( t < WAV_restore_end )
      return -WAV_undershoot * ztone( (WAV_restore_end-t)/WAV_restore_time ) ;

   return 0.0f ;
}

/*----------------------------------------------------------------*/
/* WFUN convolution for various basis functions with durations. */

#define WTYPE_SPMG1 1
#define WTYPE_SPMG2 2
#define WTYPE_SPMG3 3

#define WTYPE_GAM   7
#define WTYPE_MION  8
#define WTYPE_WAV   9
#define WTYPE_MIONN 10

#define WTYPE_TWOGAM 17

typedef struct {
  int wtype , nfun ;
  float dur , parm[9] ;
  float *fun ;
} WFUN_storage ;

#undef  WFUN_equals
#define WFUN_equals(wa,wb) ( (wa).wtype   == (wb).wtype   && \
                             (wa).dur     == (wb).dur     && \
                             (wa).parm[0] == (wb).parm[0] && \
                             (wa).parm[1] == (wb).parm[1] && \
                             (wa).parm[2] == (wb).parm[2] && \
                             (wa).parm[3] == (wb).parm[3] && \
                             (wa).parm[4] == (wb).parm[4] && \
                             (wa).parm[5] == (wb).parm[5] && \
                             (wa).parm[6] == (wb).parm[6] && \
                             (wa).parm[7] == (wb).parm[7] && \
                             (wa).parm[8] == (wb).parm[8]   )

static int          nWFUNS = 0    ;
static WFUN_storage *WFUNS = NULL ;

/* "micro time" resolution for this convolution */
#undef  WFUNDT
#define WFUNDT 0.01f

/*----------------------------------------------------------------*/
/* Return value is index in WFUNS of the function value struct. */

static int setup_WFUN_function( int wtyp , float dur , float *parm )
{
   float val,tt,vmax,vthr,aval ;
   int nlag , ii,jj,have_nz ;
   WFUN_storage ws ;
   float *hhrf=NULL ; int nhrf, ahrf;
   float (*wavfun)(float) = NULL ;
   char msg[222] ;

ENTRY("setup_WFUN_function") ;

   if( dur < 0.0f ) dur = 0.0f ;
   ws.wtype = wtyp ; ws.dur = dur ;
   ws.parm[0] = ws.parm[1] = ws.parm[2] = ws.parm[3] =
                ws.parm[4] = ws.parm[5] = ws.parm[6] =
                             ws.parm[7] = ws.parm[8] = 0.0f ;

   switch( wtyp ){

     default: RETURN(-1) ;  /* bad input */

     case WTYPE_WAV:
       if( parm != NULL ){
         WAV_delay_time   = MAX(0.0f,parm[0]) ; /* input params */
         WAV_rise_time    = MAX(0.1f,parm[1]) ;
         WAV_fall_time    = MAX(0.1f,parm[2]) ;
         WAV_undershoot   = MAX(0.0f,parm[3]) ;
         WAV_restore_time = MAX(0.1f,parm[4]) ;
       } else {
         WAV_delay_time   =  2.0f ;           /* default params */
         WAV_rise_time    =  4.0f ;
         WAV_fall_time    =  6.0f ;
         WAV_undershoot   =  0.2f ;
         WAV_restore_time =  2.0f ;
       }
       setup_WAV_constants() ;
       ws.parm[0] = WAV_rise_start  ;
       ws.parm[1] = WAV_fall_start  ;
       ws.parm[2] = WAV_fall_end    ;
       ws.parm[3] = WAV_restore_end ;
       wavfun     = waveform_WAV    ;  /* func to be integrated */

       sprintf(msg,
         "waveform setup: WAV(dur=%g,delay=%g,rise=%g,fall=%g,undershoot=%g,restore=%g)" ,
         dur,WAV_delay_time,WAV_rise_time,
         WAV_fall_time,WAV_undershoot,WAV_restore_time) ;

     break ;

     case WTYPE_SPMG1:
       wavfun = waveform_SPMG1 ;
       sprintf(msg,"waveform setup: SPMG1(dur=%g)",dur) ;
     break ; /* no params */

     case WTYPE_SPMG2:
       wavfun = waveform_SPMG2 ;
       sprintf(msg,"waveform setup: SPMG2(dur=%g)",dur) ;
     break ;

     case WTYPE_SPMG3:
       wavfun = waveform_SPMG3 ;
       sprintf(msg,"waveform setup: SPMG3(dur=%g)",dur) ;
     break ;

     case WTYPE_MION:
       wavfun = waveform_MION  ;
       sprintf(msg,"waveform setup: MION(dur=%g)",dur) ;
     break ;

     case WTYPE_MIONN:
       wavfun = waveform_MIONN  ;
       sprintf(msg,"waveform setup: MIONN(dur=%g)",dur) ;
     break ;

     case WTYPE_GAM:
       wavfun = waveform_GAM  ;
       ws.parm[0] = GAM_p = parm[0] ;
       ws.parm[1] = GAM_q = parm[1] ;
       GAM_top = GAM_p*GAM_q + 5.0f*sqrtf(GAM_p)*GAM_q ;
       sprintf(msg,"waveform setup: GAM(p=%g,q=%g,dur=%g)",GAM_p,GAM_q,dur) ;
     break ;

     case WTYPE_TWOGAM:          /* 06 Jan 2018 */
       wavfun = waveform_TWOGAM  ;
       ws.parm[0] = TWOGAM_parm[0] = parm[0] ;
       ws.parm[1] = TWOGAM_parm[1] = parm[1] ;
       ws.parm[2] = TWOGAM_parm[2] = parm[2] ;
       ws.parm[3] = TWOGAM_parm[3] = parm[3] ;
       ws.parm[4] = TWOGAM_parm[4] = parm[4] ;
       ws.parm[5] = TWOGAM_parm[5] = parm[5] ;
       ws.parm[6] = TWOGAM_top     = parm[6] ;
       sprintf(msg,"waveform setup: TWOGAM()") ;
     break ;
   }

   /* check if we have a duplicate of an existing WFUN function */

   for( ii=0 ; ii < nWFUNS ; ii++ )
     if( WFUN_equals(ws,WFUNS[ii]) ) RETURN(ii) ;  /* found a match */

   /**** must create a new WFUN function: ****/

   INFO_message(msg) ;

   /* create array of basic waveformm, convolved with a square wave */

   nlag = 1 + (int)rint(dur/WFUNDT) ;  /* number of convolution lags */

   ahrf = 1024 ; nhrf = 0 ;
   hhrf = (float *)malloc(sizeof(float)*ahrf) ;

   /* loop until we get nonzero values, then decline back to zero */

   vmax = vthr = 0.001f ;
   for( have_nz=ii=0 ; ; ii++ ){
     tt = ii*WFUNDT ;  /* ii-th output time */

     for( val=0.0f,jj=0 ; jj < nlag && tt >= 0.0f ; jj++,tt-=WFUNDT )
       val += wavfun(tt) ; /* convolution */

     aval = fabsf(val) ;
     if( aval != 0.0f ){
       have_nz++ ;
       if( aval > vmax ){ vmax = aval ; vthr = 0.0001f*vmax ; }
     } else if( ii > 99 && have_nz > 1 &&
              fabsf(hhrf[ii-1]) <= vthr && fabsf(hhrf[ii-2]) <= vthr &&
              fabsf(hhrf[ii-3]) <= vthr && fabsf(hhrf[ii-4]) <= vthr   ){
       break ;
     }

     if( ii >= ahrf ){
       ahrf += 1024 ; hhrf = (float *)realloc(hhrf,sizeof(float)*ahrf) ;
     }
     hhrf[ii] = val ;
   }
   nhrf = ii-3 ;
   hhrf = (float *)realloc(hhrf,sizeof(float)*nhrf) ;

   ws.nfun = nhrf ;  /* store array in struct */
   ws.fun  = hhrf ;

   /* scale array to have max abs value 1 */

   vmax = 1.0f / vmax ;
   for( ii=0 ; ii < nhrf ; ii++ ) hhrf[ii] *= vmax ;

   /* add struct to permanent storage */

   WFUNS = (WFUN_storage *)realloc(WFUNS,sizeof(WFUN_storage)*(nWFUNS+1)) ;
   memcpy( WFUNS+nWFUNS , &ws , sizeof(WFUN_storage) ) ;
   nWFUNS++ ;

   RETURN(nWFUNS-1) ;  /* index of new struct */
}

/*----------------------------------------------------------------*/

static float basis_WFUN( float t, float fwav, float junk1, float junk2, void *q )
{
   int iwav = (int)fwav , ii ;
   int    nhrf ;
   float *hhrf , xx ;

   if( t < 0.0f || iwav < 0 || iwav >= nWFUNS ) return 0.0f ;

   nhrf = WFUNS[iwav].nfun ;
   hhrf = WFUNS[iwav].fun  ;

   xx = t/WFUNDT ; ii = (int)xx ; xx = xx - ii ;
   if( ii >= nhrf   ) return 0.0f ;
   if( ii == nhrf-1 ) return (1.0f-xx)*hhrf[ii] ;
   return ( xx*hhrf[ii+1] + (1.0f-xx)*hhrf[ii] ) ;
}

/*==========================================================================*/
/*------------------------ End of WAV function stuff -----------------------*/
/*==========================================================================*/

/*--------------------------------------------------------------------------*/
/* Take a string and generate a basis expansion structure from it.
----------------------------------------------------------------------------*/

basis_expansion * basis_parser( char *sym )
{
   basis_expansion *be ;
   char *cpt , *scp ;
   float bot=0.0f, top=0.0f ;
   int nn , nord=0 ;

ENTRY("basis_parser") ;

   if( sym == NULL ) RETURN(NULL);

   scp = strdup(sym) ;                        /* duplicate, for editing */
   cpt = strchr(scp,'(') ;                    /* find opening '(' */
   if( cpt != NULL ){ *cpt = '\0' ; cpt++ ; } /* cut string there */

   be = (basis_expansion *)malloc(sizeof(basis_expansion)) ;
   be->name = NULL ;   /* will be fixed later */
   be->symfun = strdup(sym) ;  /* 06 Mar 2007 */
   be->modsub = NULL ;         /* 12 Jul 2012 */

   be->vmod = be->vfun = 0 ;   /* 05 Dec 2008 */

   be->no_iresp = 0 ;          /* 07 Nov 2011 */

   /*--- GAM(b,c) ---*/

   if( strcmp(scp,"GAM") == 0 || strcmp(scp,"GAMpw") == 0 ){

     int do_pq = (strcmp(scp,"GAMpw") == 0) ;

     be->nfunc = 1 ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     if( cpt == NULL ){
       be->bfunc[0].a = 8.6f ;     /* Mark Cohen's parameters */
       be->bfunc[0].b = 0.547f ;   /* t_peak=4.7 FWHM=3.7 */
       be->bfunc[0].c = 11.1f ;    /* return to zero-ish */
       be->bfunc[0].f = basis_gam ;
       be->tbot = 0.0f ; be->ttop = be->bfunc[0].c ;
     } else {
       float dur=-1.0f ;
       sscanf(cpt,"%f,%f,%f",&bot,&top,&dur) ;
       if( dur < 0.0f && (bot <= 0.0f || top <= 0.0f) ){
         ERROR_message("'GAM(%s' is illegal",cpt) ;
         ERROR_message(
           " Correct format: 'GAM(b,c)' with b > 0 and c > 0.");
         free((void *)be->bfunc); free((void *)be); free(scp); RETURN(NULL);
       }
       if( do_pq ){  /* 06 Jan 2018 */
         float_pair pq = gam_peak_fwhm_convert(bot,top) ;
         if( pq.a <= 0.0f || pq.b <= 0.0f ){
           ERROR_message("'GAMpw(%s' is illegal",cpt) ;
           free((void *)be->bfunc); free((void *)be); free(scp); RETURN(NULL);
         }
         bot = pq.a ; top = pq.b ;
       }
       if( dur < 0.0f ){   /* the olden way: no duration given */
         be->bfunc[0].a = bot ;    /* t_peak = bot*top */
         be->bfunc[0].b = top ;    /* FWHM   = 2.35*sqrt(bot)*top */
         be->bfunc[0].c = bot*top + 9.9f*sqrtf(bot)*top ;  /* long enough */
         be->bfunc[0].f = basis_gam ;
         be->tbot = 0.0f ; be->ttop = be->bfunc[0].c ;
       } else {            /* duration given ==> integrate it */
         int iwav ; float parm[2] ;
         if( bot <= 0.0f ) bot = 8.6f ;
         if( top <= 0.0f ) top = 0.547f ;
         if( dur == 0.0f ) dur = 0.01f ;
         parm[0] = bot ; parm[1] = top ;
         iwav = setup_WFUN_function( WTYPE_GAM , dur , parm ) ;
         if( iwav < 0 ){
           ERROR_message("Can't setup GAM(%f,%f,%f) for some reason?!",bot,top,dur) ;
           free((void *)be); free(scp); RETURN(NULL);
         }
         be->tbot = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;
         be->bfunc[0].f = basis_WFUN ;
         be->bfunc[0].a = (float)iwav ;
         be->bfunc[0].b = 0.0f ;
         be->bfunc[0].c = 0.0f ;
       }
     }

   /*--- TWOGAM(p1,q1,r,p2,q2[,dur]) [06 Jan 2018] ---*/

   } else if( strcmp(scp,"TWOGAM") == 0 || strcmp(scp,"TWOGAMpw") == 0 ){
     float dur=0.0f,p1=0.0f,q1=0.0f,rr=0.0f,p2=0.0f,q2=0.0f , b1,b2 ;
     float *fq=NULL ;
     int do_pq = (strcmp(scp,"TWOGAMpw") == 0) ;

     be->nfunc = 1 ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     if( cpt == NULL ){
       ERROR_message("TWOGAM format is incorrect") ;
       free((void *)be->bfunc); free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%f,%f,%f,%f",&p1,&q1,&rr,&p2,&q2,&dur) ;
     if( p1 <= 0.0f || q1 <= 0.0f || p2 <= 0.0f || q2 <= 0.0f || dur < 0.0f || fabsf(rr) > 1.0f ){
       ERROR_message("'TWOGAM(%s' is illegal",cpt) ;
       free((void *)be->bfunc); free((void *)be); free(scp); RETURN(NULL);
     }
     if( do_pq ){
       float_pair pq1 = gam_peak_fwhm_convert(p1,q1) ;
       float_pair pq2 = gam_peak_fwhm_convert(p2,q2) ;
       if( pq1.a <= 0.0f || pq1.b <= 0.0f || pq2.a <= 0.0f || pq2.b <= 0.0f ){
         ERROR_message("'TWOGAMpw(%s' is illegal",cpt) ;
         free((void *)be->bfunc); free((void *)be); free(scp); RETURN(NULL);
       }
       p1 = pq1.a ; q1 = pq1.b ; p2 = pq2.a ; q2 = pq2.b ;
     }
     fq = (float *)malloc(sizeof(float)*7) ;
     fq[0] = p1 ; fq[1] = q1 ; fq[2] = rr ;
     fq[3] = p2 ; fq[4] = q2 ; fq[5] = dur ;
     b1 = p1*q1+9.9f*sqrtf(p1)*q1 ;
     b2 = p2*q2+9.9f*sqrtf(p2)*q2 ; fq[6] = MAX(b1,b2) ;
     if( dur == 0.0f ){
       float mx=0.0f , tt,vv ;
       be->bfunc[0].a = 1.0f ;        /* scale factor */
       be->bfunc[0].b = 0.0f ;        /* ignored */
       be->bfunc[0].c = fq[6] ;       /* impulse duration */
       be->bfunc[0].f = basis_twogam ;
       be->bfunc[0].q = (void *)fq ;  /* all params */
       be->tbot = 0.0f ; be->ttop = be->bfunc[0].c ;
       /* find max value, to get scale factor */
       for( tt=WFUNDT ; tt < be->ttop ; tt+=WFUNDT ){
         vv = basis_twogam( tt , 1.0f , 0.0f , be->ttop , (void *)fq ) ;
         vv = fabsf(vv) ; if( mx < vv ) mx = vv ;
       }
       be->bfunc[0].a = 1.0f / mx ;
     } else {            /* duration given ==> integrate it */
       int iwav ;
       iwav = setup_WFUN_function( WTYPE_TWOGAM , dur , fq ) ;
       if( iwav < 0 ){
         ERROR_message("Can't setup TWOGAM(%s for some reason?!",cpt) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
       be->tbot = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;
       be->bfunc[0].f = basis_WFUN ;
       be->bfunc[0].a = (float)iwav ;
       be->bfunc[0].b = 0.0f ;
       be->bfunc[0].c = 0.0f ;
     }

   /*--- TENT(bot,top,order) ---*/  /*-- add TENTzero 23 Jul 2010 --*/

   } else if( strcmp(scp,"TENT")     == 0 ||
              strcmp(scp,"TENTzero") == 0   ){
     float dx ; int zzz = (strstr(scp,"zero") != NULL) , bb ;

     if( cpt == NULL ){
       ERROR_message("'%s' by itself is illegal",scp) ;
       ERROR_message(
        " Correct format: '%s(bot,top,n)' with bot < top and n > %d.",
        scp , (zzz) ? 2 : 1 ) ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 2 || (nord < 3 && zzz) ){
       ERROR_message("'%s(%s' is illegal",scp,cpt) ;
       ERROR_message(
        " Correct format: '%s(bot,top,n)' with bot < top and n > %d.",
        scp , (zzz) ? 2 : 1 ) ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     be->nfunc = (zzz) ? nord-2 : nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     dx        = (top-bot) / (nord-1) ;

     bb = 0 ;
     if( !zzz ){
       be->bfunc[bb].f = basis_tent ;
       be->bfunc[bb].a = bot-0.00111f*dx ;
       be->bfunc[bb].b = bot ;
       be->bfunc[bb].c = bot+dx ;
       bb++ ;
     }
     for( nn=1 ; nn < nord-1 ; nn++,bb++ ){
       be->bfunc[bb].f = basis_tent ;
       be->bfunc[bb].a = bot + (nn-1)*dx ;
       be->bfunc[bb].b = bot +  nn   *dx ;
       be->bfunc[bb].c = bot + (nn+1)*dx ;
     }
     if( !zzz ){
       be->bfunc[bb].f = basis_tent ;
       be->bfunc[bb].a = bot + (nord-2)*dx ;
       be->bfunc[bb].b = top ;
       be->bfunc[bb].c = top + 0.00111f*dx ;
    }

   /*--- CSPLIN(bot,top,order) ---*/  /*-- add CSPLINzero 23 Jul 2010 --*/

   } else if( strcmp(scp,"CSPLIN")     == 0 ||   /* 15 Mar 2007 */
              strcmp(scp,"CSPLINzero") == 0   ){
     float dx ; int zzz = (strstr(scp,"zero") != NULL) , bb , nbot,ntop ;

     if( cpt == NULL ){
       ERROR_message("'%s' by itself is illegal",scp) ;
       ERROR_message(
        " Correct format: '%s(bot,top,n)' with bot < top and n > 3.",scp) ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 4 ){
       ERROR_message("'%s(%s' is illegal",scp,cpt) ;
       ERROR_message(
        " Correct format: '%s(bot,top,n)' with bot < top and n > 3.",scp) ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     be->nfunc = (zzz) ? (nord-2) : nord ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     dx        = (top-bot) / (nord-1) ;

     if( zzz ){ nbot = 1 ; ntop = nord-2 ; }
     else     { nbot = 0 ; ntop = nord-1 ; }
     for( bb=0,nn=nbot ; nn <= ntop ; nn++,bb++ ){
       be->bfunc[bb].f = basis_csplin ;
       be->bfunc[bb].a = bot +  nn*dx ;
       be->bfunc[bb].b = dx ;
       be->bfunc[bb].c = 0.0f ;
     }
     if( zzz ){
       be->bfunc[0].c      = -1.0f ;  /* edge markers */
       be->bfunc[nord-3].c =  1.0f ;
     } else {
       be->bfunc[0].c      = -2.0f ;
       be->bfunc[1].c      = -1.0f ;
       be->bfunc[nord-2].c =  1.0f ;
       be->bfunc[nord-1].c =  2.0f ;
     }

   /*--- TRIG(bot,top,order) ---*/

   } else if( strcmp(scp,"TRIG") == 0 ){

     if( cpt == NULL ){
       ERROR_message("'TRIG' by itself is illegal") ;
       ERROR_message(
        " Correct format: 'TRIG(bot,top,n)' with bot < top and n > 2.") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 3 ){
       ERROR_message("'TRIG(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'TRIG(bot,top,n)' with bot < top and n > 2.") ;
       free((void *)be); free(scp); RETURN(NULL);
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
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 1 ){
       ERROR_message("'SIN(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'SIN(bot,top,n)' with bot < top and n > 0.") ;
       free((void *)be); free(scp); RETURN(NULL);
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
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f,%d",&bot,&top,&nord) ;
     if( bot >= top || nord < 1 || nord > POLY_MAX ){
       ERROR_message("'POLY(%s' is illegal",cpt) ;
       ERROR_message(
        " Correct format: 'POLY(bot,top,n)' with bot<top and n>0 and n<%d",
        POLY_MAX+1) ;
       free((void *)be); free(scp); RETURN(NULL);
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

   /*--- SPMG ---*/  /*--- or SPMG3 [27 Jul 2007] ---*/

   } else if( strncmp(scp,"SPMG",4) == 0 ){
     int nb ;                               /* number of basis funcs */
     float dur=-1.0f ;                      /* 28 Apr 2009: duration param */

     switch( scp[4] ){                      /* 08 Mar 2009: */
       default:
         WARNING_message("unknown '%s' defaults to 'SPMG2'",scp) ; /*fallthru*/
       case '\0':
       case '2': nb = 2 ; break ;           /* now allow nb=1..3 */
       case '3': nb = 3 ; break ;
       case '1': nb = 1 ; break ;
     }

     if( cpt != NULL ) sscanf(cpt,"%f",&dur) ; /* 28 Apr 2009: get duration param */

     if( dur < 0.0f ){    /** the olden way, with dur not given **/
       be->nfunc = nb ;                       /* no longer fixed at 2 */
       be->tbot  = 0.0f ; be->ttop = 25.0f ;
       be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
       be->bfunc[0].f = basis_spmg1 ;
       be->bfunc[0].a = 0.0f ;                /* parameters aren't used */
       be->bfunc[0].b = 0.0f ;
       be->bfunc[0].c = 0.0f ;
       if( nb >= 2 ){                         /* 08 Mar 2009 */
         be->bfunc[1].f = basis_spmg2 ;
         be->bfunc[1].a = 0.0f ;
         be->bfunc[1].b = 0.0f ;
         be->bfunc[1].c = 0.0f ;
       }
       if( nb >= 3 ){                         /* 27 Jul 2007 */
         be->bfunc[2].f = basis_spmg3 ;
         be->bfunc[2].a = 0.0f ;
         be->bfunc[2].b = 0.0f ;
         be->bfunc[2].c = 0.0f ;
       }

    } else {              /** the new way: dur given **/
      int iwav ;

      be->nfunc = nb ;
      be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;

      /* setup function #1 in WFUNS[iwav] and then into be */

      iwav = setup_WFUN_function( WTYPE_SPMG1 , dur , NULL ) ;
      if( iwav < 0 ){
        ERROR_message("Can't setup SPMG1(%f) for some reason?!",dur) ;
        free((void *)be); free(scp); RETURN(NULL);
      }

      be->tbot = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;

      be->bfunc[0].f = basis_WFUN ;
      be->bfunc[0].a = (float)iwav ;
      be->bfunc[0].b = 0.0f ;
      be->bfunc[0].c = 0.0f ;

      if( nb >= 2 ){
        iwav = setup_WFUN_function( WTYPE_SPMG2 , dur , NULL ) ;
        if( iwav < 0 ){
          ERROR_message("Can't setup SPMG2(%f) for some reason?!",dur) ;
          free((void *)be); free(scp); RETURN(NULL);
        }
        be->bfunc[1].f = basis_WFUN ;
        be->bfunc[1].a = (float)iwav ;
        be->bfunc[1].b = 0.0f ;
        be->bfunc[1].c = 0.0f ;
      }

      if( nb >= 3 ){
        iwav = setup_WFUN_function( WTYPE_SPMG3 , dur , NULL ) ;
        if( iwav < 0 ){
          ERROR_message("Can't setup SPMG3(%f) for some reason?!",dur) ;
          free((void *)be); free(scp); RETURN(NULL);
        }
        be->bfunc[2].f = basis_WFUN ;
        be->bfunc[2].a = (float)iwav ;
        be->bfunc[2].b = 0.0f ;
        be->bfunc[2].c = 0.0f ;
      }
    }

   /*--- dmBLOCKn for n=4 or 5 ; the first 'vfun' function [08 Dec 2008] ---*/

   } else if( strncmp(scp,"dmBLOCK",7) == 0 ){
     int nb=4 ; float peak=0.0f ;  /* 03 Apr 2012: alter default peak from 1 to 0 */

     if( scp[7] != '\0' ){
       nb = strtol( scp+7 , NULL , 10 ) ;
       if( nb != 4 && nb != 5 ){
         ERROR_message("'%s' has illegal power: only 4 or 5 allowed",scp) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
     }

     if( cpt != NULL ) sscanf(cpt,"%f",&peak) ; /* 30 Sep 2009: get peak param */

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = BASIS_MAX_DURATION+15.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = (nb==5) ? basis_block5_OLD : basis_block4_OLD ;
     be->bfunc[0].a = 1.0f;   /* duration: will actually come from timing file */
     be->bfunc[0].b = peak ;
     be->bfunc[0].c = 0.0f ;
     be->vfun       = 1 ;     /* needs 1 param from timing file */
     be->no_iresp   = 1 ;     /* 07 Nov 2011 */

   /*--- dmUBLOCKn for n=4 or 5 ; the first 'vfun' function [08 Dec 2008] ---*/

   } else if( strncmp(scp,"dmUBLOCK",8) == 0 ){
     int nb=4 ; float peak=0.0f ;  /* 03 Apr 2012: alter default peak from 1 to 0 */

     if( scp[8] != '\0' ){
       nb = strtol( scp+8 , NULL , 10 ) ;
       if( nb != 4 && nb != 5 ){
         ERROR_message("'%s' has illegal power: only 4 or 5 allowed",scp) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
     }

     if( cpt != NULL ) sscanf(cpt,"%f",&peak) ; /* 30 Sep 2009: get peak param */

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = BASIS_MAX_DURATION+15.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = (nb==5) ? basis_block5_NEW : basis_block4_NEW ;
     be->bfunc[0].a = 1.0f;   /* duration: will actually come from timing file */
     be->bfunc[0].b = peak ;
     be->bfunc[0].c = 0.0f ;
     be->vfun       = 1 ;     /* needs 1 param from timing file */
     be->no_iresp   = 1 ;     /* 07 Nov 2011 */

   /*--- BLOCKn(duration,peak) for n=4 or 5 ---*/

   } else if( strncmp(scp,"BLOCK",5) == 0 || strncmp(scp,"IGFUN",5) == 0 ){
     int nb=4 ;

     if( scp[5] != '\0' ){
       nb = strtol( scp+5 , NULL , 10 ) ;
       if( nb != 4 && nb != 5 ){
         ERROR_message("'%s' has illegal power: only 4 or 5 allowed",scp) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
     }

     if( cpt == NULL ){
       ERROR_message("'%s' by itself is illegal",scp) ;
       ERROR_message(
        " Correct format: 'BLOCKn(dur)' with dur > 0, n=4 or 5\n"
        "                     *OR* 'BLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f",&top,&bot) ;  /* top = duration, bot = peak */
     if( top <= 0.0f ){
       ERROR_message("'%s(%s' is illegal",scp,cpt) ;
       ERROR_message(
        " Correct format: 'BLOCKn(dur)' with dur > 0, n=4 or 4\n"
        "                      or: 'BLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     if( bot < 0.0f ) bot = 0.0f ;

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = top+15.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = (nb==5) ? basis_block5_OLD : basis_block4_OLD ;
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
       } else if( FLDIF(bot,first_block_peakval) > 0.002f ){
         WARNING_message(
          "%s has different peak value than first %s\n"
          "            We hope you know what you are doing!" ,
          sym , first_block_peaksym ) ;
       }

       if( bot == 0.0f ){
         if( first_len_pkzero == 0.0f ){
           first_len_pkzero = top ;
           first_sym_pkzero = strdup(sym) ;
         } else if( FLDIF(top,first_len_pkzero) > 0.002f ){
           WARNING_message(
            "%s has different duration than first %s\n"
            "       ==> Amplitudes will differ.  We hope you know what you are doing!" ,
            sym , first_sym_pkzero ) ;
         }
       }

       num_block++ ;
     }

   /*--- UBLOCKn(duration,peak) for n=4 or 5 ---*/

   } else if( strncmp(scp,"UBLOCK",6) == 0 ){
     int nb=4 ;

     if( scp[6] != '\0' ){
       nb = strtol( scp+6 , NULL , 10 ) ;
       if( nb != 4 && nb != 5 ){
         ERROR_message("'%s' has illegal power: only 4 or 5 allowed",scp) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
     }

     if( cpt == NULL ){
       ERROR_message("'%s' by itself is illegal",scp) ;
       ERROR_message(
        " Correct format: 'UBLOCKn(dur)' with dur > 0, n=4 or 5\n"
        "                     *OR* 'UBLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f",&top,&bot) ;  /* top = duration, bot = peak */
     if( top <= 0.0f ){
       ERROR_message("'%s(%s' is illegal",scp,cpt) ;
       ERROR_message(
        " Correct format: 'UBLOCKn(dur)' with dur > 0, n=4 or 4\n"
        "                      or: 'UBLOCKn(dur,peak)' with peak > 0 too.") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     if( bot < 0.0f ) bot = 0.0f ;

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = top+15.0f ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = (nb==5) ? basis_block5_NEW : basis_block4_NEW ;
     be->bfunc[0].a = top ;
     be->bfunc[0].b = bot ;
     be->bfunc[0].c = 0.0f ;

     /* 04 May 2007: check for consistency in UBLOCK-izing */

     { static float first_block_peakval = 0.0f ;
       static char *first_block_peaksym = NULL ;
       static int   num_block           = 0 ;
       static float first_len_pkzero    = 0.0f ;
       static char *first_sym_pkzero    = NULL ;

       if( num_block == 0 ){
         first_block_peakval = bot ;
         first_block_peaksym = strdup(sym) ;
       } else if( FLDIF(bot,first_block_peakval) > 0.002f ){
         WARNING_message(
          "%s has different peak value than first %s\n"
          "            We hope you know what you are doing!" ,
          sym , first_block_peaksym ) ;
       }

       if( bot == 0.0f ){
         if( first_len_pkzero == 0.0f ){
           first_len_pkzero = top ;
           first_sym_pkzero = strdup(sym) ;
         } else if( FLDIF(top,first_len_pkzero) > 0.002f ){
           WARNING_message(
            "%s has different duration than first %s\n"
            "       ==> Amplitudes will differ.  We hope you know what you are doing!" ,
            sym , first_sym_pkzero ) ;
         }
       }

       num_block++ ;
     }

  /*--- macro to scan for a float number and assign to a value ---*/

#undef  WSCAN
#define WSCAN(vv)                                                 \
 do{ if( dpt != NULL && *dpt != '\0' ){                           \
       if( isdigit(*dpt) ){                                       \
         (vv) = (float)strtod(dpt,&ept) ;                         \
         dpt = ept ; if( *dpt != '\0' ) dpt++ ;                   \
       } else if( *dpt == '-' ){                                  \
         ERROR_message("Can't give negative arguments to 'WAV'"); \
         free((void *)be); free(scp); RETURN(NULL);               \
       } else {                                                   \
         dpt++ ;                                                  \
       }                                                          \
     }                                                            \
   } while(0)

   /*--- WAV(dur,delaytime,risetime,falltime.undershoot,restoretime) ---*/

   } else if( strcmp(scp,"WAV") == 0 ){    /* 06 Mar 2009 */
     float parm[5] = { 2.0f , 4.0f , 6.0f , 0.2f , 2.0f } ;
     float dur = 0.0f ; char *dpt=cpt , *ept ; int iwav ;

     /* scan for up to 6 parameters */

     WSCAN(dur    ) ; WSCAN(parm[0]) ; WSCAN(parm[1]) ;
     WSCAN(parm[2]) ; WSCAN(parm[3]) ; WSCAN(parm[4]) ;

     /* setup the function in WFUNS[iwav] */

     iwav = setup_WFUN_function( WTYPE_WAV , dur , parm ) ;
     if( iwav < 0 ){
       ERROR_message("Can't setup WAV for some reason?!") ;
       free((void *)be); free(scp); RETURN(NULL);
     }

     be->nfunc = 1 ;
     be->tbot  = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     be->bfunc[0].f = basis_WFUN ;
     be->bfunc[0].a = (float)iwav ;
     be->bfunc[0].b = 0.0f ;
     be->bfunc[0].c = 0.0f ;

   /*--- EXPR(bot,top) exp1 exp2 ... ---*/

   } else if( strcmp(scp,"EXPR") == 0 ){   /* 28 Aug 2004 */
     char *ept ;
     NI_str_array *sar ;
     int nexpr , ie ;
     PARSER_code *pc ;

     if( cpt == NULL ){
       ERROR_message("'EXPR' by itself is illegal") ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     sscanf(cpt,"%f,%f",&bot,&top) ;
     if( top <= bot ){
       ERROR_message("'EXPR(%f,%f)' has illegal time range",bot,top) ;
       free((void *)be); free(scp); RETURN(NULL);
     }
     ept = strchr( cpt , ')' ) ;
     if( ept == NULL ){
       ERROR_message("'EXPR(%f,%f)' has no expressions!?",bot,top);
       free((void *)be); free(scp); RETURN(NULL);
     }
     sar = NI_decode_string_list( ept+1 , "~" ) ;
     if( sar == NULL || sar->num == 0 ){
       ERROR_message("'EXPR(%f,%f)' has no expressions after '('!?",bot,top);
       free((void *)be); free(scp); RETURN(NULL);
     }
     be->nfunc = nexpr = sar->num ;
     be->tbot  = bot  ; be->ttop = top ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;
     PARSER_set_printout(1) ;
     for( ie=0 ; ie < nexpr ; ie++ ){
       pc = PARSER_generate_code( sar->str[ie] ) ;
       if( pc == NULL ){
         ERROR_message("unparsable EXPRession '%s'",sar->str[ie]) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
       if( strcmp(sar->str[ie],"1") != 0 &&    /* must either be "1" */
           !PARSER_has_symbol("t",pc)    &&    /* or contain symbol  */
           !PARSER_has_symbol("x",pc)    &&    /* 't', 'x', or 'z'   */
           !PARSER_has_symbol("z",pc)      ){
         ERROR_message(
           "illegal EXPRession '%s' lacks symbol t, x, or z",
           sar->str[ie]) ;
         free((void *)be); free(scp); RETURN(NULL);
       }
       be->bfunc[ie].f = basis_expr ;
       be->bfunc[ie].a = bot ;
       be->bfunc[ie].b = top ;
       be->bfunc[ie].c = 1.0f/(top-bot) ;  /* for ease of scaling */
       be->bfunc[ie].q = (void *)pc ;
     }
     PARSER_set_printout(0) ;

     NI_delete_str_array(sar) ;

   /*--- MION ---*/

   } else if( strcmp(scp,"MION") == 0 ){
     float dur=-1.0f ; int iwav ;

     if( cpt != NULL ) sscanf(cpt,"%f",&dur) ; /* 28 Apr 2009: get duration param */

     be->nfunc = 1 ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;

     if( dur < 0.0f ){            /* impulse response */
       be->bfunc[0].f = basis_mion ;
       be->bfunc[0].a = 0.0f ;    /* no parameters */
       be->bfunc[0].b = 0.0f ;
       be->bfunc[0].c = 0.0f ;
       be->tbot = 0.0f ; be->ttop = 60.0f ;
     } else {                     /* convolve with square wave */
        iwav = setup_WFUN_function( WTYPE_MION , dur , NULL ) ;
        if( iwav < 0 ){
          ERROR_message("Can't setup MION(%f) for some reason?!",dur) ;
          free((void *)be); free(scp); RETURN(NULL);
        }
        be->bfunc[0].f = basis_WFUN ;
        be->bfunc[0].a = (float)iwav ;
        be->bfunc[0].b = 0.0f ;
        be->bfunc[0].c = 0.0f ;
        be->tbot = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;
     }

   /*--- MIONN ---*/

   } else if( strcmp(scp,"MIONN") == 0 ){
     float dur=-1.0f ; int iwav ;

     if( cpt != NULL ) sscanf(cpt,"%f",&dur) ; /* 28 Apr 2009: get duration param */

     be->nfunc = 1 ;
     be->bfunc = (basis_func *)calloc(sizeof(basis_func),be->nfunc) ;

     if( dur < 0.0f ){            /* impulse response */
       be->bfunc[0].f = basis_mionn ;
       be->bfunc[0].a = 0.0f ;    /* no parameters */
       be->bfunc[0].b = 0.0f ;
       be->bfunc[0].c = 0.0f ;
       be->tbot = 0.0f ; be->ttop = 60.0f ;
     } else {                     /* convolve with square wave */
        iwav = setup_WFUN_function( WTYPE_MIONN , dur , NULL ) ;
        if( iwav < 0 ){
          ERROR_message("Can't setup MION(%f) for some reason?!",dur) ;
          free((void *)be); free(scp); RETURN(NULL);
        }
        be->bfunc[0].f = basis_WFUN ;
        be->bfunc[0].a = (float)iwav ;
        be->bfunc[0].b = 0.0f ;
        be->bfunc[0].c = 0.0f ;
        be->tbot = 0.0f ; be->ttop = WFUNDT * WFUNS[iwav].nfun ;
     }


   /*--- NO MORE BASIS FUNCTION CHOICES ---*/

   } else {
     ERROR_message("'%s' is unknown response function type",scp) ;
     free((void *)be); free(scp); RETURN(NULL);
   }

   /* 28 Apr 2005: set scaling factors */

   for( nn=0 ; nn < be->nfunc ; nn++ )  /* initialize factors to 1.0 */
     be->bfunc[nn].ffac = 1.0 ;

   /* for each basis function, find its peak value, then
      set ffac so that the peak value becomes basis_normall */

#undef  BNSUB
#define BNSUB 1999
   if( basis_normall > 0.0f ){
     int jj , jtop=BNSUB ; float dt , ftop , val ;
     bot = be->tbot ; top = be->ttop ; dt = (top-bot)/BNSUB ;
     if( dt < 0.01f * basis_TR ){        /* should be rare */
       dt = 0.01f * basis_TR ; jtop = 1 + (int)((top-bot)/dt) ;
     } else if( dt > 0.1f * basis_TR ){  /* should not happen */
       dt = 0.10f * basis_TR ; jtop = 1 + (int)((top-bot)/dt) ;
     }
     for( nn=0 ; nn < be->nfunc ; nn++ ){
       ftop = 0.0f ;
       for( jj=0 ; jj <= jtop ; jj++ ){
         val = basis_funceval( be->bfunc[nn] , bot+jj*dt ) ;
         val = fabs(val) ; if( val > ftop ) ftop = val ;
       }
       if( ftop > 0.0f ) be->bfunc[nn].ffac = basis_normall / ftop ;
     }
   }

   free(scp); RETURN(be);
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
   char label[512] ;
   const float EPSILON = 1.0e-10 ;

   /* open input 3D+time dataset to get some parameters */

   if( be->no_iresp ){  /* 07 Nov 2011 */
     ERROR_message("Can't use -iresp for %s %s (%s)",be->option,be->symfun,be->name) ;
     return ;
   }

   in_dset = THD_open_dataset(option_data->input_filename);
   CHECK_OPEN_ERROR(in_dset,option_data->input_filename);

  if( option_data->force_TR > 0.0 )   /* 18 Aug 2008 */
    EDIT_dset_items( in_dset ,
                       ADN_ttdel , option_data->force_TR ,
                       ADN_ntt   , DSET_NVALS(in_dset) ,
                       ADN_tunits, UNITS_SEC_TYPE ,
                     ADN_none ) ;

   nvox    = in_dset->daxes->nxx * in_dset->daxes->nyy * in_dset->daxes->nzz;
   DSET_UNMSEC(in_dset) ;  /* 12 Aug 2005 */

   if( dt <= 0.0f ) dt = DSET_TR(in_dset) ;
   if( dt <= 0.0f ) dt = 1.0f ;

   /* create output dataset on the input as a model, then close the input */

   out_dset = EDIT_empty_copy( in_dset ) ;
   tross_Copy_History( in_dset , out_dset ) ;
   DSET_delete( in_dset ) ;

   /* historicize the output */

   sprintf( label , "Impulse response: %s" , output_filename ) ;
   if( commandline != NULL )
     tross_multi_Append_History( out_dset , commandline,label,NULL ) ;
   else
     tross_Append_History ( out_dset, label);

   ts_length = 1 + (int)myceil( (be->ttop - be->tbot)/dt ) ; /* 13 Apr 2005: +1 */

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
   if( !THD_ok_overwrite() &&                /* ZSS: Dec. 16 08 */
        THD_deconflict_prefix(out_dset) > 0 )  {
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename, DSET_PREFIX(out_dset) ) ;
   }
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

       EDIT_misfit_report( DSET_FILECODE(out_dset) , ib ,
                           nvox , factor , bar , hout[ib] ) ;

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
/*! For IRFs defined by -stim_times basis function expansion, write out
    a 1D dataset with time spacing dt.
------------------------------------------------------------------------*/

void basis_write_iresp_1D( int argc , char *argv[] ,
                        DC_options *option_data ,
                        basis_expansion *be , float dt ,
                        float **wtar , char *output_filename )
{
   int nvox, ii, nf, allz, ts_length ;
   register int pp, ib ;
   float *wt , *tt , **hout , factor , **bb ;
   short *bar ;
   char label[512] ;
   const float EPSILON = 1.0e-10 ;

   if( be->no_iresp ){  /* 07 Nov 2011 */
     ERROR_message("Can't use -iresp for %s %s (%s)",be->option,be->symfun,be->name) ;
     return ;
   }

   nvox = 1 ;  /* from basis_write_iresp(), but use 1 voxel */

   if( dt <= 0.0f ) dt = 1.0f ;

   ts_length = 1 + (int)myceil( (be->ttop - be->tbot)/dt ) ; /* 13 Apr 2005: +1 */

   /* create output bricks */

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

   /* write output */
   write_one_ts( output_filename , ts_length , hout ) ;

   /* and free results */
   for( ib=0 ; ib < ts_length ; ib++ )
       free((void *)hout[ib]) ;
   free((void *)hout) ;

   if( verb )
    INFO_message("Wrote iresp 1D dataset into %s\n",output_filename) ;

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
   char label[512] ;
   const float EPSILON = 1.0e-10 ;

   /* open input 3D+time dataset to get some parameters */

   if( be->no_iresp ){  /* 07 Nov 2011 */
     ERROR_message("Can't use -sresp for %s %s (%s)",be->option,be->symfun,be->name) ;
     return ;
   }

   in_dset = THD_open_dataset(option_data->input_filename);
   CHECK_OPEN_ERROR(in_dset,option_data->input_filename);

  if( option_data->force_TR > 0.0 )   /* 18 Aug 2008 */
    EDIT_dset_items( in_dset ,
                       ADN_ttdel , option_data->force_TR ,
                       ADN_ntt   , DSET_NVALS(in_dset) ,
                       ADN_tunits, UNITS_SEC_TYPE ,
                     ADN_none ) ;

   nvox    = in_dset->daxes->nxx * in_dset->daxes->nyy * in_dset->daxes->nzz;
   DSET_UNMSEC(in_dset) ;  /* 12 Aug 2005 */

   if( dt <= 0.0f ) dt = DSET_TR(in_dset) ;
   if( dt <= 0.0f ) dt = 1.0f ;

   /* create output dataset on the input as a model, then close the input */

   out_dset = EDIT_empty_copy( in_dset ) ;
   tross_Copy_History( in_dset , out_dset ) ;
   DSET_delete( in_dset ) ;

   /* historicize the output */

   sprintf( label , "Sigma response: %s" , output_filename ) ;
   if( commandline != NULL )
     tross_multi_Append_History( out_dset , commandline,label,NULL ) ;
   else
     tross_Append_History ( out_dset, label);

   ts_length = 1 + (int)myceil( (be->ttop - be->tbot)/dt ) ;

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
   if( !THD_ok_overwrite() &&       /* ZSS: Dec. 16 08 */
         THD_deconflict_prefix(out_dset) > 0 ) {
    WARNING_message("Filename conflict: changing '%s' to '%s'",
                    output_filename,DSET_PREFIX(out_dset) ) ;
   }
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

   bb = (float **) malloc( sizeof(float *) * nf ) ;
   for( pp=0 ; pp < nf ; pp++ ){
     bb[pp] = (float *) malloc( sizeof(float) * ts_length ) ;
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

       EDIT_misfit_report( DSET_FILECODE(out_dset) , ib ,
                           nvox , factor , bar , hout[ib] ) ;

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
/* similarly, make 1D version of write_sresp        19 Aug 2011 [rickr] */
void basis_write_sresp_1D( int argc , char *argv[] ,
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
   char label[512] ;
   const float EPSILON = 1.0e-10 ;

   if( be->no_iresp ){  /* 07 Nov 2011 */
     ERROR_message("Can't use -sresp for %s %s (%s)",be->option,be->symfun,be->name) ;
     return ;
   }

   nvox = 1 ; /* from basis_write_sresp(), but use 1 voxel */

   if( option_data->force_TR > 0.0 ) dt = 1.0f ;
   if( dt <= 0.0f ) dt = 1.0f ;

   ts_length = 1 + (int)myceil( (be->ttop - be->tbot)/dt ) ;

   /* create output bricks */

   hout = (float **) malloc( sizeof(float *) * ts_length ) ;
   for( ib=0 ; ib < ts_length ; ib++ )
     hout[ib] = (float *)calloc(sizeof(float),nvox) ;

   nf = be->nfunc ;
   tt = (float *) malloc( sizeof(float) * ts_length ) ;

   for( ib=0 ; ib < ts_length ; ib++ )     /* output time grid */
     tt[ib] = be->tbot + ib*dt ;

   /* evaluate basis vectors for output on dt time grid */

   bb = (float **) malloc( sizeof(float *) * nf ) ;
   for( pp=0 ; pp < nf ; pp++ ){
     bb[pp] = (float *) malloc( sizeof(float) * ts_length ) ;
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

   /* write output */
   write_one_ts( output_filename , ts_length , hout ) ;

   /* and free results */
   for( ib=0 ; ib < ts_length ; ib++ )
       free((void *)hout[ib]) ;
   free((void *)hout) ;

   if( verb )
    INFO_message("Wrote sresp 1D dataset into %s\n",output_filename) ;

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
      ERROR_message("----- !! %s matrix condition:  UNDEFINED ** VERY BAD **\n" ,
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
        INFO_message("----- %s matrix condition [%s] (%dx%d):  %g  %s\n",
                xname, (use_psinv) ? "X" : "XtX",
                xdata.rows,xdata.cols , cond , rating ) ;
      else
        WARNING_message("----- !! %s matrix condition [%s] (%dx%d):  %g  %s\n",
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

/*----------------------------------------------------------------------------*/
/* Function to convert FSL style stim times image of 3 columns
   to -stim_times_AM1 style of vector image.
   Note that the input (and output) are row major.  In the input,
   each row is one stimulus time; unlike FSL, the file can specify times
   for more than one run -- runs are separated by a row of '*'.
   In the output, each row is one run, and each entry has 3 values
   (vdim==3): time,amplitude,duration.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * convert_FSL_to_fvect( MRI_IMAGE *fslim , int do_amp1 )
{
   int ncol , nrun , qq,qf,qd , nfx,nfy , irun,itim ;
   MRI_IMAGE *nbt ;
   float *far , *nbar , stim,sdur,samp ;

   if( fslim == NULL || fslim->vdim > 0 ) return NULL ;

   nfx = fslim->nx ;  /* number of values per row, should be 3 */
   nfy = fslim->ny ;  /* number of rows in input file */

   far = MRI_FLOAT_PTR(fslim) ;

   /* count number of runs (separated by rows of * = bigger than big_time) */

   for( ncol=nrun=qq=qf=0 ; qq < nfy ; qq++ ){
     if( far[qq*nfx] > big_time ){
       nrun++ ; qd = qq-qf ; ncol = MAX(qd,ncol) ; qf = qq+1 ;
     }
   }
   nrun++ ; qd = nfy-qf ; ncol = MAX(qd,ncol) ;

#undef  NBP
#define NBP(ir,ic) (nbar + 3*((ir)*ncol+(ic)) )

   nbt  = mri_new_fvectim( ncol , nrun , 1 , 3 ) ;
   nbar = (float *)nbt->im ;
   for( qq=0 ; qq < 3*ncol*nrun ; qq++ ) nbar[qq] = basis_filler ;

   for( irun=qq=0,qf=-1 ; qq < nfy ; qq++ ){
     stim = far[qq*nfx] ;
     if( stim > big_time ){ irun++ ; qf = qq ; continue ; }
     sdur = (nfx < 2)            ? 0.1f : far[qq*nfx+1] ;
     samp = (nfx < 3 || do_amp1) ? 1.0f : far[qq*nfx+2] ;
     NBP(irun,qq-qf-1)[0] = stim ;
     NBP(irun,qq-qf-1)[1] = samp ;
     NBP(irun,qq-qf-1)[2] = sdur ;
   }

   return nbt ;
}

/******************************************************************************/
/******************************************************************************/
#if 0
/*--- The shell script below is for testing this ARMA/REML implementation. ---*/

#!/bin/tcsh

### Script to test REML GLSQ vs OLSQ regression

# B      = signal amplitude for all repetitions
# P      = signal period (TRs)
# nstim  = number of signals (IM regression)
# numvox = number of voxels to simulate
# so there is a total of $nstim * $numvox stimuli being simulated

set B      = 2
set P      = 12
set nstim  = 20
set numvox = 400

# ARMA(1,1) parameters for this test/simulation

set AA  = 0.8
set LAM = 0.5

# D = number of time points (TR=1)

@ D = $P * $nstim

# create stimulus timing

1deval -num $nstim -expr "i*${P}"  > stim.1D

# create the voxel time series = simulated data

1deval -num $D -expr "${B}*sin(PI*t/${P})^2"  > signal.1D
foreach ii ( `count -dig 4 1 $numvox` )
  1dgenARMA11 -num $D -a $AA -lam $LAM               > noise.1D
  1deval      -a noise.1D -b signal.1D -expr 'a+b'   > data${ii}.1D
end

# glue them together into one file

1dcat data0*.1D > data.1D
\rm -f data0*.1D noise.1D signal.1D

# create the regression matrix

3dDeconvolve -num_stimts 1                                            \
             -stim_times_IM 1 stim.1D "EXPR(0,${P}) sin(PI*t/${P})^2" \
             -stim_label    1 'sinsq'                                 \
             -nodata $D 1 -x1D_stop -polort 2 -x1D test.xmat.1D

# analyses

3dREMLfit -matrix test.xmat.1D \
          -input data.1D\'     \
          -Rvar  test.Rvar.1D  \
          -Rbeta test.Rbeta.1D \
          -Obeta test.Obeta.1D \
          -nobout -Grid 5 -MAXa 0.9 -MAXb 0.9 -NEGcor

# extract the betas for each voxel into one long single column 1D file
# instead of the multi-column file output by 3dREMLfit

@ ns1 = $nstim - 1
if( -f test.Rbeta.all.1D ) \rm test.Rbeta.all.1D
if( -f test.Obeta.all.1D ) \rm test.Obeta.all.1D
foreach ii ( `count -dig 1 0 $ns1` )
  1dcat test.Rbeta.1D"[$ii]" >> test.Rbeta.all.1D
  1dcat test.Obeta.1D"[$ii]" >> test.Obeta.all.1D
end

# compute the mean and stdev of the GLSQ and OLSQ betas
# (means should be about B, or something weird happened)

3dTstat -mean -stdev -prefix test.Rbeta.stat.1D test.Rbeta.all.1D\'
3dTstat -mean -stdev -prefix test.Obeta.stat.1D test.Obeta.all.1D\'

# compute the ratio of the stdevs
# srat > 1 means OLSQ stdev was bigger than GLSQ (what we expect)

set Rsig = `1dcat test.Rbeta.stat.1D'[1]'`
set Osig = `1dcat test.Obeta.stat.1D'[1]'`
set srat = `ccalc "$Osig/$Rsig"`

# print out these results

echo "======================="
echo "a = $AA  lam = $LAM"
echo "REML mean stdev = " `1dcat test.Rbeta.stat.1D`
echo "OLSQ mean stdev = " `1dcat test.Obeta.stat.1D`
echo "Osig/Rsig       =  $srat"
echo "======================="

time ; exit 0
#endif
/******************************************************************************/
/******************************************************************************/
