/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1998-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Plugin to calculate the deconvolution of a measured 3d+time dataset
  with a specified input stimulus time series.  Output consists of the
  least squares estimate of the impulse response function, and the fitted 
  time series (input stimulus convolved with estimated impulse response
  function).

  File:    plug_deconvolve.c
  Author:  B. Douglas Ward
  Date:    09 September 1998

  Mod:     Generate error message for time series of insufficient length.
  Date:    29 October 1998

  Mod:     Restructured matrix calculations to improve execution speed.
  Date:    16 December 1998

  Mod:     Minor correction to stim_label option.
  Date:    17 December 1998

  Mod:     Removed restriction on length of input time series.
  Date:    31 December 1998

  Mod:     Accept mean square error from full model.
  Date:    04 January 1999

  Mod:     Earlier termination if unable to invert X matrix.
           (Avoids redundant error messages.)
  Date:    06 January 1999

  Mod:     Change NLast default value.
  Date:    27 May 1999

  Mod:     Added option for matrix calculation of general linear tests.
  Date:    02 July 1999

  Mod:     Increased max. allowed number of input stimulus functions.
  Date:    24 August 1999

  Mod:     Additional statistical output (partial R^2 statistics).
  Date:    07 September 1999

  Mod:     Allow reading of multiple input stimulus functions from a single
           file by selection of individual columns.
  Date:    09 November 1999

  Mod:     Modifications for compatibility with 3dDeconvolve options for
           writing the fitted full model time series (-fitts) and the 
           residual error time series (-errts) to 3d+time datasets.
  Date:    22 November 1999

  Mod:     Added test for maximum number of full model parameters.
  Date:    24 November 1999

  Mod:     Small change to order of print out to screen.
  Date:    29 November 1999

  Mod:     Increased maximum number of GLT matrices and linear constraints.
  Date:    03 January 2000

  Mod:     Modified matrix_file_read to use mri_read_ascii routine.
  Date:    12 January 2000

  Mod:     Added censor input option to allow operator to eliminate individual
           time points from the analysis.
  Date:    21 June 2000

  Mod:     Added screen display of p-values.
  Date:    22 June 2000

  Mod:     Added -glt_label option for labeling the GLT matrix.
  Date:    23 June 2000

  Mod:     Added Concat Runs option for analysis of a concatenated 3d+time 
           dataset.
  Date:    27 June 2000

  Mod:     Increased size of screen output buffer.
  Date:    27 July 2000

  Mod:     Increased maximum number of linear constraints.
  Date:    07 December 2000

  Mod:     Changes required for compatibility with -nodata option 
           (norm.std.dev.'s for GLT linear constraints).
  Date:    21 December 2000

  Mod:     Added NPTR option, to allow input stim functions to be sampled
           at a multiple of the 1/TR rate.
  Date:    02 January 2001 

  Mod:     Increased maximum degree of polynomial ort.
  Date:    16 May 2001

  Mod:     Removed automatic override of NFirst option.
  Date:    08 June 2001

  Mod:     Enhanced screen output:  Display of p-values for individual stim
           function regression coefficients.  Display of t-stats and p-values
           for individual linear constraints within a GLT.
  Date:    29 January 2002

  Mod:     Allow user to specify no baseline parameters in the model 
           by setting Baseline to "None".           
  Date:    26 February 2002

  Mod:     Allow user to specify which input stimulus functions are part of
           the baseline model.
  Date:    02 May 2002

  Mod:     Improved graphical representation of the estimated impulse response
           function when the user selects option DC_IRF under Tran 1D of the 
           graph options menu.
  Date:    06 May 2002

  Mod:     Fixed bug in show_options rountine that would cause program crash 
           when the baseline was set to "None".
  Date:    03 Oct 2002

  Mod:     Increased size of screen output buffer (again).
  Date:    28 October 2002

  Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     Additional input error testing for -censor and -concat options.
  Date:    17 March 2003
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "plug_deconvolve"            /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "09 September 1998"  /* initial program release date */
#define PROGRAM_LATEST  "18 March 2003"      /* latest program revision date */

/*---------------------------------------------------------------------------*/

#include "afni.h"
#include "matrix.h"

#define MAX_NAME_LENGTH THD_MAX_NAME    /* max. string length for file names */
#define MAX_XVARS 250                           /* max. number of parameters */
#define MAX_STIMTS 20                 /* max. number of stimulus time series */
#define MAX_GLT 20                    /* max. number of general linear tests */
#define MAX_CONSTR 20                 /* max. number of linear constraints   */

#define RA_error DC_error

/*---------------------------------------------------------------------------*/
/*
   Print error message and return.
*/

static void DC_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);

}


/*---------------------------------------------------------------------------*/

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif


/*------------- string to 'help' the user -------------*/

static char helpstring[] =
   " Purpose: Control DC_Fit, DC_Err, and DC_IRF  Deconvolution Functions.  \n"
   "                                                                        \n"
   " Control      Base      = Baseline polynomial: None, Constant, Linear,  \n"
   "                            Quadratic, Cubic, Quartic, or Quintic       \n"
   "              NFirst    = Number of first time series point to use      \n"
   "              NLast     = Number of last time series point to use       \n"
   "              IRF       = Label of stimulus fnc. to use for IRF plot    \n"
   "                                                                        \n"
   " Concat       Label     = Name to use as label for concatenation        \n"
   "              File      = File containing list of volume indices of     \n"
   "                            starting points for individual runs         \n"
   "              Col #     = Column of file which contains concat run list \n"
   "                                                                        \n"
   " Censor       Label     = Name to use as label for censor function      \n"
   "              File      = Time series file indicating censored points   \n"
   "              Col #     = Column of file which contains censor function \n"
   "                                                                        \n"
   " StimFnc      Label     = Name to use as label for this input stimulus  \n"
   "              File      = Time series file representing input stimulus  \n"
   "              Col #     = Column of file which contains input stimulus  \n"
   "              MinLag    = Minimum time delay for impulse response       \n"
   "              MaxLag    = Maximum time delay for impulse response       \n"
   "              NPTR      = Number of stim fn. time points per TR         \n"
   "              Base      = Is this input stimulus part of baseline model?\n"
   "                                                                        \n"
   " GLT Mat      Label     = Name to use as label for this GLT matrix      \n"
   "              File      = File containing the GLT matrix                \n"
   "              # Rows    = Number of rows (linear constraints) in GLT    \n"
   "                                                                        \n"
   "                                                                        \n"
   " For more details, see:                                                 \n"
   " `Deconvolution Analysis of FMRI Time Series Data' by B. Douglas Ward,  \n"
   " which is contained in file 3dDeconvolve.ps of the AFNI distribution.   \n"
;


/*------- Strings for baseline control ------*/

#define NBASE 7
static char * baseline_strings[NBASE] = {"None", "Const", "Linear", 
			    "Quadrtc", "Cubic", "Quartic", "Quintic" };

static char * false_or_true[2] = {"False", "True"};

/*--------------- prototypes for internal routines ---------------*/

static char * DC_main( PLUGIN_interface * ) ;  /* the entry point */

static void DC_Fit (int nt, double to, double dt, float * vec, char ** label) ;
static void DC_Err (int nt, double to, double dt, float * vec, char ** label) ;
static void DC_IRF (int nt, double to, double dt, float * vec, char ** label) ;
static int calculate_results();



/*---------------- global data -------------------*/

static PLUGIN_interface * global_plint = NULL;

static int plug_polort;     /* degree of polynomial for baseline model */
static int plug_p;          /* total number of parameters in the full model */
static int plug_q;          /* total number of parameters in the base model */
static int plug_qp;         /* number of poly. trend baseline parameters */
static int plug_NFirst;     /* first image from input 3d+time dataset to use */
static int plug_NLast;      /* last image from input 3d+time dataset to use */
static int plug_IRF;        /* which impulse response fuction to plot */
static int initialize;      /* flag for perform initialization */
static int prev_nt;         /* previous time series length */
static char * IRF_label;    /* label of stimulus for IRF plot */

static char * concat_label;      /* label for concatenation */
static int concat_column;        /* column containing list of blocks (runs) */
static int num_blocks;           /* number of blocks (runs) */
static int * block_list;         /* list of block (run) starting points */

static int num_censor;                /* flag for include censor function */
static char * censor_label;           /* censor time series label */
static int censor_column;             /* column containing censor array */
static float * censor_array;          /* censor time series array */
static int censor_length;             /* length of censor time series */
static int * good_list;               /* list of usable time points */

static int num_stimts;                     /* number of stimulus time series */
static int stim_base[MAX_STIMTS];          /* flag for baseline stimulus */
static int stim_column[MAX_STIMTS];        /* column containing stimulus */
static float * stimulus[MAX_STIMTS];       /* stimulus time series arrays */
static int stim_length[MAX_STIMTS];        /* length of stimulus time series */
static int min_lag[MAX_STIMTS];   /* minimum time delay for impulse response */
static int max_lag[MAX_STIMTS];   /* maximum time delay for impulse response */
static int nptr[MAX_STIMTS];      /* number of stim fn. time points per TR */
static char * stim_label[MAX_STIMTS];      /* stimulus time series labels */

static matrix xdata;              /* independent variable matrix */
static matrix x_full;             /* extracted X matrix   for full model */
static matrix xtxinv_full;        /* matrix:  1/(X'X)     for full model */
static matrix xtxinvxt_full;      /* matrix:  (1/(X'X))X' for full model */
static matrix x_base;             /* extracted X matrix   for baseline model */
static matrix xtxinvxt_base;      /* matrix:  (1/(X'X))X' for baseline model */
static matrix x_rdcd[MAX_STIMTS]; /* extracted X matrices for reduced models */
static matrix xtxinvxt_rdcd[MAX_STIMTS];     
                                  /* matrix:  (1/(X'X))X' for reduced models */

static int glt_num;                 /* number of general linear tests */
static char * glt_label[MAX_GLT];   /* general linear test labels */
static int glt_rows[MAX_GLT];       /* number of linear constraints in glt */
static char * glt_filename[MAX_GLT];/* file containing glt matrix */
static matrix cxtxinvct[MAX_GLT];   /* matrices: C(1/(X'X))C' for GLT */
static matrix glt_cmat[MAX_GLT];    /* general linear test matrices */
static matrix glt_amat[MAX_GLT];    /* constant GLT matrices for later use */
static vector glt_coef[MAX_GLT];    /* linear combinations from GLT matrices */
static vector glt_tcoef[MAX_GLT];   /* t-stats for GLT linear combinations */


/*---------------------------------------------------------------------------*/
/*
  Include deconvolution analysis software.
*/

#undef USE_BASIS          /* 11 Aug 2004: for Deconvolve.c */

#include "Deconvolve.c"



/*---------------------------------------------------------------------------*/
/*
  Initialize control options and global data.
*/

static void initialize_options ()
{
  int is;                     /* input stimulus index */
  int iglt;                   /* index for general linear test */


  /*----- Initialize control parameters -----*/
  plug_polort = 1;        /* degree of polynomial for baseline model */
  plug_p      = 0;        /* total number of parameters in the full model */
  plug_q      = 0;        /* total number of parameters in the base model */
  plug_qp     = 0;        /* number of poly. trend baseline parameters */
  plug_NFirst = 0;        /* first image from input 3d+time dataset to use */
  plug_NLast  = 32767;    /* last image from input 3d+time dataset to use */
  plug_IRF    = -1;       /* which impulse response fuction to plot */
  initialize  = 0;        /* flag for perform initialization */
  prev_nt     = 0;        /* previous time series length */
  IRF_label   = malloc (sizeof(char)*MAX_NAME_LENGTH);   MTEST (IRF_label);
  strcpy (IRF_label, " ");      /* label of stimulus for IRF plot */


  /*----- Initialization for concatenated runs -----*/
  concat_label = malloc (sizeof(char)*MAX_NAME_LENGTH);  MTEST (concat_label);
  strcpy (concat_label, " ");    /* label for concatenation */
  concat_column = 0;             /* column containing list of blocks (runs) */
  num_blocks = 1;                /* number of blocks (runs) */
  block_list = (int *) malloc (sizeof(int) * 1);  MTEST (block_list);  
  block_list[0] = 0;             /* list of block (run) starting points */


  /*----- Initialize censorship function -----*/
  num_censor = 0;
  censor_label = malloc (sizeof(char)*MAX_NAME_LENGTH);  MTEST (censor_label);
  strcpy (censor_label, " ");        /* censor time series label */
  censor_column = 0;                 /* column containing censor array */
  censor_array = NULL;               /* censor time series array */
  censor_length = 0;                 /* length of censor time series */
  good_list = NULL;                  /* list of usable time points */


  /*----- Initialize stimulus functions -----*/
  num_stimts = 0;                          /* number of stimulus time series */
  for (is =0;  is < MAX_STIMTS;  is++)
    {
      stim_label[is] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (stim_label[is]);
      sprintf (stim_label[is], "Stim #%d ", is+1);
                                           /* stimulus time series labels */
      stim_base[is] = 0;                   /* flag for baseline stimulus */
      stim_column[is] = 0;                 /* column containing stimulus */
      stimulus[is] = NULL;                 /* stimulus time series arrays */
      stim_length[is] = 0;                 /* length of stimulus time series */
      min_lag[is] = 0;            /* minimum time delay for impulse response */
      max_lag[is] = 0;            /* maximum time delay for impulse response */
      nptr[is] = 1;               /* number of stim fn. time points per TR */
   }


  /*----- Initialize matrices -----*/
  matrix_initialize (&xdata);         /* independent variable matrix */
  matrix_initialize (&x_full);        /* extracted X matrix   for full model */
  matrix_initialize (&xtxinv_full);   /* matrix:  1/(X'X)     for full model */
  matrix_initialize (&xtxinvxt_full); /* matrix:  (1/(X'X))X' for full model */
  matrix_initialize (&x_base);    /* extracted X matrix   for baseline model */
  matrix_initialize (&xtxinvxt_base);
                                  /* matrix:  (1/(X'X))X' for baseline model */
  for (is =0;  is < MAX_STIMTS;  is++)
    {
      matrix_initialize (&x_rdcd[is]); 
                                  /* extracted X matrices for reduced models */
      matrix_initialize (&xtxinvxt_rdcd[is]);
                                  /* matrix:  (1/(X'X))X' for reduced models */
    }


  /*----- Initialize GLT matrices -----*/
  glt_num = 0;                             /* number of general linear tests */
  for (iglt =0;  iglt < MAX_GLT;  iglt++)
    {
      glt_label[iglt] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (glt_label[iglt]);
      sprintf (glt_label[iglt], "GLT #%d ", iglt+1);
                                               /* general linear test labels */
      glt_rows[iglt] = 0;             /* number of linear constraints in glt */
      glt_filename[iglt] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (glt_filename[iglt]);
      strcpy (glt_filename[iglt], " ");        /* file containing glt matrix */

      matrix_initialize (&cxtxinvct[iglt]);
                                           /* matrices: C(1/(X'X))C' for GLT */
      matrix_initialize (&glt_cmat[iglt]);
                                             /* general linear test matrices */
      matrix_initialize (&glt_amat[iglt]); 
                                      /* constant GLT matrices for later use */
      vector_initialize (&glt_coef[iglt]);
                                    /* linear combinations from GLT matrices */
      vector_initialize (&glt_tcoef[iglt]);
                                    /* t-stats for GLT linear combinations   */
    }

}


/*---------------------------------------------------------------------------*/
/*
  Reset control options and global data.
*/

static void reset_options ()
{
  int is;                     /* input stimulus index */
  int iglt;                   /* index for general linear test */


  /*----- Reset control parameters -----*/
  plug_polort = 1;        /* degree of polynomial for baseline model */
  plug_p      = 0;        /* total number of parameters in the full model */
  plug_q      = 0;        /* total number of parameters in the base model */
  plug_qp     = 0;        /* number of poly. trend baseline parameters */
  plug_NFirst = 0;        /* first image from input 3d+time dataset to use */
  plug_NLast  = 32767;    /* last image from input 3d+time dataset to use */
  plug_IRF    = -1;       /* which impulse response fuction to plot */
  initialize  = 0;        /* flag for perform initialization */
  prev_nt     = 0;        /* previous time series length */
  strcpy (IRF_label, " ");       /* label of stimulus for IRF plot */


  /*----- Reset for concatenated runs -----*/
  strcpy (concat_label, " ");    /* label for concatenation */
  concat_column = 0;             /* column containing list of blocks (runs) */
  num_blocks = 1;                /* number of blocks (runs) */
  if (block_list != NULL)  free (block_list);       
  block_list = (int *) malloc (sizeof(int) * 1);  MTEST (block_list);  
  block_list[0] = 0;             /* list of block (run) starting points */


  /*----- Reset censorship function -----*/
  num_censor = 0;
  strcpy (censor_label, " ");        /* censor time series label */
  censor_column = 0;                 /* column containing censor array */
  if (censor_array != NULL)
    {  
      free (censor_array);   
      censor_array = NULL;           /* censor time series array */
    }
  censor_length = 0;                 /* length of censor time series */
  if (good_list != NULL)
    {
      free (good_list);
      good_list = NULL;              /* list of usable time points */
    }


  /*----- Reset stimulus functions -----*/
  num_stimts = 0;                          /* number of stimulus time series */
  for (is =0;  is < MAX_STIMTS;  is++)
    {
      sprintf (stim_label[is], "Stim #%d ", is+1);
                                           /* stimulus time series labels */
      stim_base[is] = 0;                   /* flag for baseline stimulus */
      stim_column[is] = 0;                 /* column containing stimulus */
      if (stimulus[is] != NULL)
	{
	  free (stimulus[is]);
	  stimulus[is] = NULL;             /* stimulus time series arrays */
	}
      stim_length[is] = 0;                 /* length of stimulus time series */
      min_lag[is] = 0;            /* minimum time delay for impulse response */
      max_lag[is] = 0;            /* maximum time delay for impulse response */
      nptr[is] = 1;               /* number of stim fn. time points per TR */
   }


  /*----- Destroy matrices -----*/
  matrix_destroy (&xdata);         /* independent variable matrix */
  matrix_destroy (&x_full);        /* extracted X matrix   for full model */
  matrix_destroy (&xtxinv_full);   /* matrix:  1/(X'X)     for full model */
  matrix_destroy (&xtxinvxt_full); /* matrix:  (1/(X'X))X' for full model */
  matrix_destroy (&x_base);       /* extracted X matrix   for baseline model */
  matrix_destroy (&xtxinvxt_base);
                                  /* matrix:  (1/(X'X))X' for baseline model */
  for (is =0;  is < MAX_STIMTS;  is++)
    {
      matrix_destroy (&x_rdcd[is]); 
                                  /* extracted X matrices for reduced models */
      matrix_destroy (&xtxinvxt_rdcd[is]);
                                  /* matrix:  (1/(X'X))X' for reduced models */
    }


  /*----- Destroy GLT matrices -----*/
  glt_num = 0;                             /* number of general linear tests */
  for (iglt =0;  iglt < MAX_GLT;  iglt++)
    {
      sprintf (glt_label[iglt], "GLT #%d ", iglt+1);
                                               /* general linear test labels */
      glt_rows[iglt] = 0;             /* number of linear constraints in glt */
      strcpy (glt_filename[iglt], " ");        /* file containing glt matrix */

      matrix_destroy (&cxtxinvct[iglt]);
                                           /* matrices: C(1/(X'X))C' for GLT */
      matrix_destroy (&glt_cmat[iglt]);
                                             /* general linear test matrices */
      matrix_destroy (&glt_amat[iglt]); 
                                      /* constant GLT matrices for later use */
      vector_destroy (&glt_coef[iglt]);
                                    /* linear combinations from GLT matrices */
      vector_destroy (&glt_tcoef[iglt]);
                                    /* t-stats for GLT linear combinations   */
    }

}


/*---------------------------------------------------------------------------*/
/*
   Set up the interface to the user.
*/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int is;                        /* input stimulus index */
   int iglt;                   /* index for general linear test */


   if( ncall > 0 ) return NULL ;  /* generate interface for ncall 0 */


   /***** do interface #0 *****/

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface ("Deconvolution" ,
	   "Control DC_Fit, DC_Err, and DC_IRF Deconvolution Functions" ,
	   helpstring, PLUGIN_CALL_VIA_MENU, DC_main);

   global_plint = plint ;  /* make global copy */

   PLUTO_short_choose(plint) ;  /* 29 Mar 2002 [RWCox]: */
   PLUTO_short_number(plint) ;  /* make 'Choose' and number widgets shorter */

   PLUTO_add_hint (plint, 
     "Control DC_Fit, DC_Err, and DC_IRF Deconvolution Functions");

   PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*----- Parameters -----*/
   PLUTO_add_option (plint, "Control", "Control", TRUE);
   PLUTO_add_string (plint, "Base", NBASE, baseline_strings, 2);
   PLUTO_add_number (plint, "NFirst", -1, 32767, 0, -1, TRUE);
   PLUTO_add_number (plint, "NLast",  0, 32767, 0, 32767,  TRUE);
   PLUTO_add_string( plint, "IRF ",    0, NULL, 1);


   /*----- Concatenation Function -----*/
   PLUTO_add_option (plint, "Concat", "Concat", FALSE);
   PLUTO_add_string( plint, "Label", 0, NULL, 1);
   PLUTO_add_timeseries (plint, "File");
   PLUTO_add_number (plint, "Col #", 0, 100, 0, 0, TRUE);


   /*----- Censor Function -----*/
   PLUTO_add_option (plint, "Censor", "Censor", FALSE);
   PLUTO_add_string( plint, "Label", 0, NULL, 1);
   PLUTO_add_timeseries (plint, "File");
   PLUTO_add_number (plint, "Col #", 0, 100, 0, 0, TRUE);


   /*----- Input Stimulus -----*/
   for (is = 0;  is < MAX_STIMTS;  is++)
     {
       PLUTO_add_option (plint, "StimFnc", "StimFnc", FALSE);
       PLUTO_add_string( plint, "Label", 0, NULL, 1);
       PLUTO_add_timeseries (plint, "File");
       PLUTO_add_number (plint, "Col #", 0, 100, 0, 0, TRUE);
       PLUTO_add_number (plint, "MinLag", 0, 100, 0, 0, TRUE);
       PLUTO_add_number (plint, "MaxLag", 0, 100, 0, 0, TRUE);
       PLUTO_add_number (plint, "NPTR",    1, 100, 0, 0, TRUE);
       PLUTO_add_string (plint, "Base", 2, false_or_true, 0);
     }

   /*----- General Linear Test -----*/
   for (is = 0;  is < MAX_GLT;  is++)
     {
       PLUTO_add_option (plint, "GLT Mat", "GLT Mat", FALSE);
       PLUTO_add_string( plint, "Label", 0, NULL, 1);
       PLUTO_add_string( plint, "File", 0, NULL, 1);     
       PLUTO_add_number (plint, "# Rows", 1, MAX_CONSTR, 0, 0, TRUE);
     }

   /*--------- done with interface setup ---------*/
   PLUTO_register_1D_funcstr ("DC_Fit" , DC_Fit);
   PLUTO_register_1D_funcstr ("DC_Err" , DC_Err);
   PLUTO_register_1D_funcstr ("DC_IRF" , DC_IRF);
   

   /*----- Initialize options and global data -----*/
   initialize_options ();


   return plint ;
}


/*---------------------------------------------------------------------------*/

static void show_options ()
{
  int ib;                         /* block (run) index */
  int it;                         /* time index */
  int is;                         /* stimulus index */
  int iglt;                       /* general linear test index */


  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");


  /*----- Show current input options -----*/
  printf ("\nControls: \n");
  printf ("Baseline  = %10s \n", baseline_strings[plug_polort+1]);
  printf ("NFirst    = %10d \n", plug_NFirst);
  printf ("NLast     = %10d \n", plug_NLast);
  printf ("IRF label = %10s \n", IRF_label);


  /*----- Identify concatenation function -----*/
  if (num_blocks > 0)
    {
      printf ("\n");
      printf ("Concatenation:     Label = %8s ", concat_label);
      printf ("Column = %3d  \n", concat_column);
      for (ib = 0;  ib < num_blocks;  ib++)
	printf ("Run #%d  Initial Point = %d \n", ib+1, block_list[ib]); 
    }


  /*----- Identify censor function -----*/
  if (num_censor > 0)
    {
      printf ("\n");
      printf ("Censor Function:   Label = %8s ", censor_label);
      printf ("Column = %3d  \n", censor_column);
      printf ("Censored Points: ");
      for (it = 0;  it < censor_length;  it++)
	{
	  if (censor_array[it] == 0)  printf (" %d", it);
	}
      printf ("\n");
    }


  /*----- List stimulus functions -----*/
  if (num_stimts > 0)
    {
      printf ("\n");
      for (is = 0;  is < num_stimts;  is++)
	{
	  if (stim_base[is])
	    printf ("Baseline:      Label = %8s ", stim_label[is]);
	  else
	    printf ("Stimulus:      Label = %8s ", stim_label[is]);
	  printf ("Column = %3d   Min. Lag = %3d   Max. Lag = %3d   ", 
		  stim_column[is], min_lag[is], max_lag[is]);
	  printf ("NPTR = %d \n", nptr[is]);
	}
    }


  /*----- List GLT matrices -----*/
  if (glt_num > 0)
    {
      printf ("\n");
      for (iglt = 0;  iglt < glt_num;  iglt++)
	{
	  printf ("GLT:       Label = %8s   ", glt_label[iglt]);
   	  printf ("#Rows = %2d   Input File: %s \n", 
		  glt_rows[iglt], glt_filename[iglt]);
	}
    }
 
}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
*/

static char * DC_main( PLUGIN_interface * plint )
{
  char * str;                           /* input string */
  int is;                               /* stimulus index */
  int iglt;                             /* general linear test index */
  MRI_IMAGE * stim;     /* pointers to image structures 
                           -- used to read 1D ASCII */
  float * far;          /* pointer to MRI_IMAGE floating point data */
  int ipt;              /* time point index */
  

  /*----- Reset options and global data -----*/
  reset_options ();


  /*--------- go to Control input line ---------*/
  PLUTO_next_option (plint);
  str    = PLUTO_get_string (plint);
  plug_polort = PLUTO_string_index (str, NBASE, baseline_strings) - 1;
  plug_NFirst = PLUTO_get_number (plint);
  plug_NLast  = PLUTO_get_number (plint);
  strcpy (IRF_label, PLUTO_get_string (plint));
	  

  /*--------- go to Concat Runs input line ---------*/
  str = PLUTO_peek_optiontag (plint);
  if (str != NULL) 

    /*----- Read Concat Runs input Line -----*/
    if (strcmp (str, "Concat") == 0)
      {
	str = PLUTO_get_optiontag (plint);
	strcpy (concat_label, PLUTO_get_string(plint));

	stim = PLUTO_get_timeseries(plint) ;
	
	if (stim == NULL || stim->nx < 1 
	    ||  stim->kind != MRI_float)
	  return "**************************\n"
	    "Illegal Concat File Input!\n"
	    "**************************"  ;
	
	/*----- Column in file which contains the concat function -----*/
	concat_column = PLUTO_get_number(plint);
	
	if ((concat_column < 0) 
	    ||(concat_column > stim->ny - 1))
	  return "**********************************\n"
	    "Illegal Concat File Column Number!\n"
	    "**********************************"  ;

	/*----- Extract concat run start list from MRI data structure -----*/
	far = MRI_FLOAT_PTR(stim);
	num_blocks = stim->nx;
	if (block_list != NULL)  free (block_list);
	block_list = (int *) malloc (sizeof(int) * num_blocks);
	MTEST (block_list);
	
	for (ipt = 0;  ipt < num_blocks;  ipt++)
	  block_list[ipt] = floor (far[ipt+concat_column*(stim->nx)] + 0.5); 
      }
  

  /*--------- go to Censor input line ---------*/
  str = PLUTO_peek_optiontag (plint);
  if (str != NULL) 

    /*----- Read Censor input Line -----*/
    if (strcmp (str, "Censor") == 0)
      {
	str = PLUTO_get_optiontag (plint);
	strcpy (censor_label, PLUTO_get_string(plint));

	stim = PLUTO_get_timeseries(plint) ;
	
	if (stim == NULL || stim->nx < 3 
	    ||  stim->kind != MRI_float)
	  return "**************************\n"
	    "Illegal Censor File Input!\n"
	    "**************************"  ;

	
	/*----- Column in file which contains the censor function -----*/
	censor_column = PLUTO_get_number(plint);
	
	if ((censor_column < 0) 
	    ||(censor_column > stim->ny - 1))
	  return "**********************************\n"
	    "Illegal Censor File Column Number!\n"
	    "**********************************"  ;


	/*----- Extract censor time series from MRI data structure -----*/
	if (censor_array != NULL) 
	  {
	    free (censor_array);
	    censor_array = NULL;
	  }
	far = MRI_FLOAT_PTR(stim);
	censor_length = stim->nx;
	censor_array = (float *) malloc (sizeof(float) * (stim->nx));
	MTEST (censor_array);
	
	num_censor = 1;
	for (ipt = 0;  ipt < (stim->nx);  ipt++)
	  censor_array[ipt] 
	    = far[ipt + censor_column*(stim->nx)]; 
      }
  

  /*------ Loop over input line(s) -----*/
  do
    {
      str = PLUTO_get_optiontag(plint); 
      if (str == NULL)  break;
      if ((strcmp (str, "StimFnc") != 0) && (strcmp (str, "GLT Mat") != 0))
        return "************************\n"
               "Illegal optiontag found!\n"
               "************************";
     

      /*----- Read Input Stimulus Line -----*/
      if (strcmp (str, "StimFnc") == 0)
	{      
	  str =  PLUTO_get_string(plint);
	  if (strlen(str) != 0)  strcpy (stim_label[num_stimts], str);

	  if (strcmp(stim_label[num_stimts], IRF_label) == 0)
	    plug_IRF = num_stimts;
	  
	  stim = PLUTO_get_timeseries(plint) ;
	  
	  if (stim == NULL || stim->nx < 3 
	      ||  stim->kind != MRI_float)
	    return "*************************\n"
	           "Illegal Timeseries Input!\n"
	           "*************************"  ;


	  /*----- Column in file which contains the stimulus function -----*/
	  stim_column[num_stimts] = PLUTO_get_number(plint);

	  if ((stim_column[num_stimts] < 0) 
	    ||(stim_column[num_stimts] > stim->ny - 1))
	    return "********************************\n"
	           "Illegal Stim File Column Number!\n"
	           "********************************"  ;


	  /*----- Extract stimulus time series from MRI data structure -----*/
	  if (stimulus[num_stimts] != NULL) 
	    {
	      free (stimulus[num_stimts]);
	      stimulus[num_stimts] = NULL;
	    }
	  far = MRI_FLOAT_PTR(stim);
	  stim_length[num_stimts] = stim->nx;
	  stimulus[num_stimts] = (float *) malloc (sizeof(float) * (stim->nx));
	  MTEST (stimulus[num_stimts]);

	  for (ipt = 0;  ipt < (stim->nx);  ipt++)
	    stimulus[num_stimts][ipt] 
	      = far[ipt + stim_column[num_stimts]*(stim->nx)]; 


	  /*----- Minimum and Maximum time lags for model -----*/
	  min_lag[num_stimts] = PLUTO_get_number(plint);
	  max_lag[num_stimts] = PLUTO_get_number(plint);
	  nptr[num_stimts]    = PLUTO_get_number(plint);
	  str    = PLUTO_get_string (plint);
	  stim_base[num_stimts] = PLUTO_string_index (str, 2, false_or_true);

	  
	  if (min_lag[num_stimts] > max_lag[num_stimts])
	    return "**************************\n"
                   "Require Min Lag <= Max Lag\n"
	           "**************************"  ;
	  
	  num_stimts++;
	}


      /*----- Read General Matrix Test Line -----*/
      if (strcmp (str, "GLT Mat") == 0)
	{      
	  str =  PLUTO_get_string(plint);
	  if (strlen(str) != 0)  strcpy (glt_label[glt_num], str);

	  strcpy (glt_filename[glt_num], PLUTO_get_string(plint));

	  glt_rows[glt_num] = PLUTO_get_number(plint);
      
	  glt_num++;
	}

    }

  while (1);


  /*----- Determine total number of parameters in the model -----*/
  plug_qp = (plug_polort + 1) * num_blocks;
  plug_q = plug_qp;
  plug_p = plug_qp;
  for (is = 0;  is < num_stimts;  is++)
    {
      if (stim_base[is])  plug_q += max_lag[is] - min_lag[is] + 1;
      plug_p += max_lag[is] - min_lag[is] + 1;
      if (plug_p > MAX_XVARS)
	{ 
	  return "****************************\n"
	    "Too many parameters in model \n"
	    "****************************"  ;
	}
    }
 

  /*----- Read the general linear test matrices -----*/
  if (glt_num > 0)
    for (iglt = 0;  iglt < glt_num;  iglt++)
      {
	matrix_file_read (glt_filename[iglt],
			  glt_rows[iglt],
			  plug_p,
			  &(glt_cmat[iglt]), 0);
	if (glt_cmat[iglt].elts == NULL)
	  { 
	    return "**************************\n"
                   "Unable to read GLT matrix \n"
	           "**************************"  ;
	  }
      } 


  /*----- Show the user input options -----*/
  show_options ();


  /*--- nothing left to do until data arrives ---*/
  initialize = 1 ;  /* successful initialization */
  prev_nt = 0;      /* previous time series length */
  
  return NULL ;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the impulse response function and associated statistics.
*/

static int calculate_results 
(
  int nt,               /* number of time points */
  double dt,            /* delta time */
  float * vec,          /* input measured data vector */
  int * NN,             /* number of usable data points */
  int * nfit,           /* number of fit parameters */
  float * fit,          /* fit parameters (regression coefficients) */
  char ** label,        /* string containing statistical summary of results */
  float ** fitts,       /* full model fitted time series */
  float ** errts        /* full model residual error time series */

)
  
{
  float * ts_array;           /* array of measured data for one voxel */

  int N;                      /* number of usable data points */
  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the baseline model */
  int qp;                     /* number of poly. trend baseline parameters */
  int m;                      /* parameter index */
  int n;                      /* data point index */

  vector coef;                /* regression parameters */
  vector scoef;               /* std. devs. for regression parameters */
  vector tcoef;               /* t-statistics for regression parameters */
  float fpart[MAX_STIMTS];    /* partial F-statistics for the stimuli */
  float rpart[MAX_STIMTS];    /* partial R^2 stats. for the stimuli */
  float ffull;                /* full model F-statistic */
  float rfull;                /* full model R^2 statistic */
  float mse;                  /* mean square error from full model */

  vector y;                   /* vector of measured data */       

  int NFirst;            /* first image from input 3d+time dataset to use */
  int NLast;             /* last image from input 3d+time dataset to use */
  int i, it;             /* data point index */
  int is;                /* stimulus index */

  float rms_min = 0.0;   /* minimum variation in data to fit full model */
  int ok;                /* flag for successful matrix calculation */
  int novar;             /* flag for insufficient variation in data */
  float fglt[MAX_GLT];   /* F-statistics for the general linear tests */
  float rglt[MAX_GLT];   /* R^2 statistics for the general linear tests */

  int ib;                /* block (run) index */
  int irb;               /* time index relative to start of block (run) */


  /*----- Check initialization flag -----*/
  if (! initialize)  return (0);


  /*----- Initialize vectors -----*/
  vector_initialize (&coef);
  vector_initialize (&scoef);
  vector_initialize (&tcoef);
  vector_initialize (&y);
  

  /*----- Initialize local variables -----*/
  qp = plug_qp;
  q = plug_q;
  p = plug_p;
  *nfit = p;


  /*----- Allocate memory for fitted time series and residuals -----*/
  *fitts    = (float *) malloc (sizeof(float) * nt);    MTEST (*fitts);
  *errts    = (float *) malloc (sizeof(float) * nt);    MTEST (*errts);


  /*----- Check length of censor array -----*/
  if ((num_censor != 0) && (censor_length < nt))
    {
      DC_error ("Input censor time series file is too short");
      return (0);
    }


  /*----- Check validity of concatenated runs list -----*/
  for (ib = 0;  ib < num_blocks;  ib++)
    if ((block_list[ib] < 0) || (block_list[ib] >= nt))
      {
	DC_error ("Invalid concatenated runs list");
	return (0);
      }
  if (num_blocks > 1)
    for (ib = 1;  ib < num_blocks;  ib++)
      if (block_list[ib] <= block_list[ib-1])
	{
	  DC_error ("Invalid concatenated runs list");
	  return (0);
	}
    

  /*----- Create list of good (usable) data points -----*/
  good_list = (int *) malloc (sizeof(int) * nt);  MTEST (good_list);
  NFirst = plug_NFirst;
  if (NFirst < 0)
    for (is = 0;  is < num_stimts;  is++)
      if (NFirst < (max_lag[is]+nptr[is]-1)/nptr[is])  
	NFirst = (max_lag[is]+nptr[is]-1)/nptr[is];
  NLast = plug_NLast;   

  N = 0;
  ib = 0;
  for (it = block_list[0];  it < nt;  it++)
    {
      if (ib+1 < num_blocks)
	if (it >= block_list[ib+1])  ib++;
      
      irb = it - block_list[ib];
	  
      if ((irb >= NFirst) && (irb <= NLast))
	if ((num_censor == 0) || (censor_array[it]))
	  {
	    good_list[N] = it;
	    N++;
	  }
    }

  if (N == 0)
    {
      DC_error ("No usable time points?");
      return (0);
    }
  if (N <= p)  
    {
      DC_error ("Insufficient time series data for deconvolution fit");
      return (0);
    }

  *NN = N;


  /*----- Perform initialization only if something has changed -----*/
  if (nt == prev_nt)
    {
      ok = 1;
    }
  else
    {
      /*----- Initialize the independent variable matrix -----*/
      demean_base = (plug_polort >= 0) ;
      ok = init_indep_var_matrix (p, qp, plug_polort, nt, N, good_list, 
				  block_list, num_blocks, num_stimts, stimulus,
				  stim_length, min_lag, max_lag, nptr, stim_base, &xdata);

      
      /*----- Initialization for the regression analysis -----*/
      if (ok)
	ok = init_regression_analysis (p, qp, num_stimts, stim_base, min_lag, 
			max_lag, xdata, &x_full, &xtxinv_full, &xtxinvxt_full,
			&x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);


      /*----- Initialization for the general linear test analysis -----*/
      if (ok)
	if (glt_num > 0)
	  ok = init_glt_analysis (xtxinv_full, glt_num, glt_cmat, glt_amat, 
				  cxtxinvct);
    }
      
  if (ok)
    {
      /*----- Extract Y-data for this voxel -----*/
      vector_create (N, &y);
      ts_array = vec;
      for (i = 0;  i < N;  i++)
	y.elts[i] = ts_array[good_list[i]];
      
      
      /*----- Perform the regression analysis for this voxel-----*/
      regression_analysis (N, p, q, num_stimts, min_lag, max_lag,
			   x_full, xtxinv_full, xtxinvxt_full, x_base,
			   xtxinvxt_base, x_rdcd, xtxinvxt_rdcd, 
			   y, rms_min, &mse, &coef, &scoef, &tcoef, 
			   fpart, rpart, &ffull, &rfull, &novar, 
			   *fitts, *errts);
      
 	  
      /*----- Perform the general linear tests for this voxel -----*/
      if (glt_num > 0)
	glt_analysis (N, p, x_full, y, mse*(N-p), coef, novar, cxtxinvct,
		      glt_num, glt_rows, glt_cmat, glt_amat, 
		      glt_coef, glt_tcoef, fglt, rglt);
      
     
      /*----- Save the fit parameters -----*/
      vector_to_array (coef, fit);
  
      
      /*----- Report results for this voxel -----*/
      printf ("\nResults for Voxel: \n");
      report_results (N, qp, q, p, plug_polort, block_list, num_blocks, 
		      num_stimts, stim_label, stim_base, min_lag, max_lag,
		      coef, tcoef, fpart, rpart, ffull, rfull, mse, 
		      glt_num, glt_label, glt_rows, glt_coef, 
		      glt_tcoef, fglt, rglt, label);
      printf ("%s \n", *label);

      prev_nt = nt;
    }

  else
    {
      vector_create (p, &coef);
      vector_to_array (coef, fit);
      strcpy (lbuf, "");
      *label = lbuf;
      prev_nt = 0;
    }

  
  /*----- Dispose of vectors -----*/
  vector_destroy (&y);
  vector_destroy (&tcoef);
  vector_destroy (&scoef);
  vector_destroy (&coef);


  if (ok)  return (1);
  else     return (0);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the multiple linear regression least squares fit.
*/

static void DC_Fit (int nt, double to, double dt, float * vec, char ** label)

{
  int N;              /* first image from input 3d+time dataset to use */
  int n;                   /* time index */
  int ifit;                /* parameter index */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int ok;                  /* Boolean for successful calculation */
  float * fitts = NULL;    /* full model fitted time series */
  float * errts = NULL;    /* full model residual error time series */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &N, &nfit, fit, label,
			  &fitts, &errts);


  /*----- If unable to complete the calculation, return all zeros -----*/
  if (!ok)
    for (n = 0;  n < nt;  n++)  vec[n] = 0.0;


  /*----- Use full model fit to the time series data -----*/
  else
    {
      for (n = 0;  n < N;  n++)
	vec[good_list[n]] = fitts[n];
    }


  /*----- Deallocate memory -----*/
  free (fitts);   fitts = NULL;
  free (errts);   errts = NULL;

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the multiple linear regression residuals.
*/

static void DC_Err (int nt, double to, double dt, float * vec, char ** label)

{
  int N;              /* first image from input 3d+time dataset to use */
  float val;               /* residual value at a time point */ 
  int n;                   /* time index */
  int ifit;                /* parameter index */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int ok;                  /* Boolean for successful calculation */
  float * fitts = NULL;    /* full model fitted time series */
  float * errts = NULL;    /* full model residual error time series */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &N, &nfit, fit, label,
			  &fitts, &errts);


  /*----- If unable to complete the calculation, return all zeros -----*/
  for (n = 0;  n < nt;  n++)  vec[n] = 0.0;


  /*----- Use residuals from full model fit to time series data -----*/
  if (ok)
    {
      for (n = 0;  n < N;  n++)
	vec[good_list[n]] = errts[n];
    }


  /*----- Deallocate memory -----*/
  free (fitts);   fitts = NULL;
  free (errts);   errts = NULL;

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the system impulse response function.
*/
 
static void DC_IRF (int nt, double to, double dt, float * vec, char ** label)

{
  int N;              /* first image from input 3d+time dataset to use */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int np;                  /* length of estimated impulse reponse function */
  int ip;                  /* impulse response function parameter index */
  int q;                   /* number of parameters in the baseline model */
  int it;                  /* array index */
  int ntdnp;               /* number of array points per IRF parameter */  
  int ok;                  /* Boolean for successful calculation */
  float * fitts = NULL;    /* full model fitted time series */
  float * errts = NULL;    /* full model residual error time series */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &N, &nfit, fit, label,
			  &fitts, &errts);


  /*----- If unable to complete the calculation, return all zeros -----*/
  if (!ok || (num_stimts < 1))
    for (it = 0;  it < nt;  it++)  vec[it] = 0.0;


  /*----- Plot the system impulse response function -----*/
  else
    {
      if ((num_stimts == 1) || (plug_IRF < 0) || (plug_IRF >= num_stimts))
	plug_IRF = 0;
      
      np = max_lag[plug_IRF] - min_lag[plug_IRF] + 1;
      
      q = plug_qp;
      for (ip = 0;  ip < plug_IRF;  ip++)
	q += max_lag[ip] - min_lag[ip] + 1;

      if (np == 1)
	{
	  for (it = 0;  it < nt;  it++)
	    vec[it] = fit[q];
	}
      else
	{
	  float r;

	  ntdnp = nt / (np-1);
      
	  vec[0] = fit[q];
	  for (it = 0;  it < nt;  it++)
	    {
	      ip = it / ntdnp + 1;
	      if (ip < np)
		{
		  r = (float) it / (float) ntdnp - (ip-1);
		  vec[it] = r * fit[q+ip] + (1.0-r) * fit[q+ip-1]; 
		}
	      else
		vec[it] = fit[q+np-1];
	    }
	}
    }


  /*----- Deallocate memory -----*/
  free (fitts);   fitts = NULL;
  free (errts);   errts = NULL;
  
  return;
}


/*---------------------------------------------------------------------------*/














