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


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "plug_deconvolve"               /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "07 September 1999"         /* date of last program mod */

/*---------------------------------------------------------------------------*/

#define MAX_NAME_LENGTH 80              /* max. streng length for file names */
#define MAX_XVARS 200                           /* max. number of parameters */
#define MAX_STIMTS 20                 /* max. number of stimulus time series */
#define MAX_GLT 10                    /* max. number of general linear tests */
#define MAX_CONSTR 10                 /* max. number of linear constraints   */

#define RA_error DC_error

#include "afni.h"
#include "matrix.h"


/*---------------------------------------------------------------------------*/
/*
   Print error message and return.
*/

void DC_error (char * message)
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
   " Control:     Baseline  = 'Constant', 'Linear', or 'Quadratic'          \n"
   "                           Is baseline 'a', 'a+bt', or 'a+bt+ct^2' ?    \n"
   "              NFirst    = Number of first time series point to use.     \n"
   "              NLast     = Number of last time series point to use.      \n"
   "              IRF Label = Which stimulus to use for generating IRF plot.\n"
   "                                                                        \n"
   " Stimulus:    Label     = Name for reference to this input stimulus.    \n"
   "              File      = Time series file representing input stimulus. \n"
   "              Min Lag   = Minimum time delay for impulse response.      \n"
   "              Max Lag   = Maximum time delay for impulse response.      \n"
;


/*------- Strings for baseline control ------*/

#define NBASE 3
static char * baseline_strings[NBASE] = {"Constant", "Linear", "Quadratic"};

/*--------------- prototypes for internal routines ---------------*/

char * DC_main( PLUGIN_interface * ) ;  /* the entry point */

void DC_Fit ();
void DC_Err ();
void DC_IRF ();
int calculate_results();



/*---------------- global data -------------------*/

static PLUGIN_interface * global_plint = NULL;

static int plug_polort=1;   /* degree of polynomial for baseline model */
static int plug_p = 0;      /* total number of parameters in the model */
static int plug_NFirst=0;   /* first image from input 3d+time dataset to use */
static int plug_NLast=32767;/* last image from input 3d+time dataset to use */
static int plug_IRF=-1;     /* which impulse response fuction to plot */
static int initialize=0;    /* flag for perform initialization */
static int prev_nt=0;       /* previous time series length */

static int num_stimts = 0;                 /* number of stimulus time series */
static char * stim_label[MAX_STIMTS];      /* stimulus time series labels */
static MRI_IMAGE * stimulus[MAX_STIMTS];   /* stimulus time series arrays */
static int min_lag[MAX_STIMTS];   /* minimum time delay for impulse response */
static int max_lag[MAX_STIMTS];   /* maximum time delay for impulse response */

static matrix xdata;              /* independent variable matrix */
static matrix x_full;             /* extracted X matrix   for full model */
static matrix xtxinv_full;        /* matrix:  1/(X'X)     for full model */
static matrix xtxinvxt_full;      /* matrix:  (1/(X'X))X' for full model */
static matrix x_base;             /* extracted X matrix   for baseline model */
static matrix xtxinvxt_base;      /* matrix:  (1/(X'X))X' for baseline model */
static matrix x_rdcd[MAX_STIMTS]; /* extracted X matrices for reduced models */
static matrix xtxinvxt_rdcd[MAX_STIMTS];     
                                  /* matrix:  (1/(X'X))X' for reduced models */

static int glt_num = 0;           /* number of general linear tests */
static int glt_rows[MAX_GLT];     /* number of linear constraints in glt */
static char * glt_filename[MAX_GLT];       /* file containing glt matrix */

static matrix glt_cmat[MAX_GLT];  /* general linear test matrices */
static matrix glt_amat[MAX_GLT];  /* constant GLT matrices for later use */
static vector glt_coef[MAX_GLT];  /* linear combinations from GLT matrices */


/*---------------------------------------------------------------------------*/
/*
  Include deconvolution analysis software.
*/

#include "Deconvolve.c"


/*---------------------------------------------------------------------------*/
/*
   Set up the interface to the user.
*/

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

   PLUTO_add_hint (plint, 
     "Control DC_Fit, DC_Err, and DC_IRF Deconvolution Functions");

   PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

   /*----- Parameters -----*/
   PLUTO_add_option (plint, "Control", "Control", TRUE);
   PLUTO_add_string (plint, "Baseline", NBASE, baseline_strings, 1);
   PLUTO_add_number (plint, "NFirst", 0, 32767, 0, 0, TRUE);
   PLUTO_add_number (plint, "NLast",  0, 32767, 0, 32767,  TRUE);
   PLUTO_add_string( plint, "IRF Label",    0, NULL, 1);


   /*----- Input Stimulus -----*/
   for (is = 0;  is < MAX_STIMTS;  is++)
     {
       PLUTO_add_option (plint, "Stimulus", "Stimulus", FALSE);
       PLUTO_add_string( plint, "Label", 0, NULL, 1);
       PLUTO_add_timeseries (plint, "File");
       PLUTO_add_number (plint, "Min Lag", 0, 100, 0, 0, TRUE);
       PLUTO_add_number (plint, "Max Lag", 0, 100, 0, 0, TRUE);
      
     }

   /*----- General Linear Test -----*/
   for (is = 0;  is < MAX_GLT;  is++)
     {
       PLUTO_add_option (plint, "GLT Matrix", "GLT Matrix", FALSE);
       PLUTO_add_number (plint, "Rows", 1, 10, 0, 0, TRUE);
       PLUTO_add_string( plint, "File", 0, NULL, 1);     
     }

   /*--------- done with interface setup ---------*/
   PLUTO_register_1D_funcstr ("DC_Fit" , DC_Fit);
   PLUTO_register_1D_funcstr ("DC_Err" , DC_Err);
   PLUTO_register_1D_funcstr ("DC_IRF" , DC_IRF);


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
      stim_label[is] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (stim_label[is]);
      strcpy (stim_label[is], " ");
    }

  for (iglt =0;  iglt < MAX_GLT;  iglt++)
    {
      glt_rows[iglt] = 0;
      matrix_initialize (&glt_cmat[iglt]);
      matrix_initialize (&glt_amat[iglt]);
      vector_initialize (&glt_coef[iglt]);
      glt_filename[iglt] = malloc (sizeof(char)*MAX_NAME_LENGTH);
      MTEST (glt_filename[iglt]);
      strcpy (glt_filename[iglt], " ");
    }


   return plint ;
}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
*/

char * DC_main( PLUGIN_interface * plint )
{
  char * str;                           /* input string */
  int is;                               /* stimulus index */
  char IRF_label[MAX_NAME_LENGTH];      /* label of stimulus for IRF plot */
  int iglt;                             /* general linear test index */
  

  /*----- reset flag for successful initialization -----*/
  initialize = 0;
    

  /*--------- go to Control input line ---------*/
  PLUTO_next_option (plint);
  str    = PLUTO_get_string (plint);
  plug_polort = PLUTO_string_index (str, NBASE, baseline_strings);
  plug_NFirst = PLUTO_get_number (plint);
  plug_NLast  = PLUTO_get_number (plint);
  strcpy (IRF_label, PLUTO_get_string (plint));


  /*------ read input line(s) -----*/
  plug_IRF = -1;
  num_stimts = 0;
  glt_num = 0;

  do
    {
      str = PLUTO_get_optiontag(plint); 
      if (str == NULL)  break;
      if ((strcmp (str, "Stimulus") != 0) && (strcmp (str, "GLT Matrix") != 0))
        return "************************\n"
               "Illegal optiontag found!\n"
               "************************";
     

      /*----- Read Input Stimulus Line -----*/
      if (strcmp (str, "Stimulus") == 0)
	{      
	  strcpy (stim_label[num_stimts], PLUTO_get_string(plint));
	  if (strcmp(stim_label[num_stimts], IRF_label) == 0)
	    plug_IRF = num_stimts;
	  
	  stimulus[num_stimts] = PLUTO_get_timeseries(plint) ;
	  
	  if (stimulus[num_stimts] == NULL || stimulus[num_stimts]->nx < 3 
	      ||  stimulus[num_stimts]->kind != MRI_float)
	    return "*************************\n"
	           "Illegal Timeseries Input!\n"
	           "*************************"  ;
	  
	  min_lag[num_stimts] = PLUTO_get_number(plint);
	  max_lag[num_stimts] = PLUTO_get_number(plint);
	  
	  if (min_lag[num_stimts] > max_lag[num_stimts])
	    return "**************************\n"
                   "Require Min Lag <= Max Lag\n"
	           "**************************"  ;
	  
	  num_stimts++;
	}


      /*----- Read General Matrix Test Line -----*/
      if (strcmp (str, "GLT Matrix") == 0)
	{      
	  glt_rows[glt_num] = PLUTO_get_number(plint);

	  strcpy (glt_filename[glt_num], PLUTO_get_string(plint));
      
	  glt_num++;
	}

    }

  while (1);


  /*----- show current input options -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\nControls: \n");
  printf ("Baseline  = %10s \n", baseline_strings[plug_polort]);
  printf ("NFirst    = %10d \n", plug_NFirst);
  printf ("NLast     = %10d \n", plug_NLast);
  printf ("IRF label = %10s \n", IRF_label);

  for (is = 0;  is < num_stimts;  is++)
    {
      printf ("\nStimulus:  %s \n", stim_label[is]);
      printf ("Min. Lag =%3d   Max. Lag =%3d \n", min_lag[is], max_lag[is]);
    }
 

  /*----- Determine total number of parameters in the model -----*/
  plug_p = plug_polort + 1;
  for (is = 0;  is < num_stimts;  is++)
    plug_p += max_lag[is] - min_lag[is] + 1;
 

  /*----- Read the general linear test matrices -----*/
  if (glt_num > 0)
    for (iglt = 0;  iglt < glt_num;  iglt++)
      {
	printf ("\nGLT #%d   #rows = %d   from file: %s \n", 
		iglt+1, glt_rows[iglt], glt_filename[iglt]);
	matrix_file_read (glt_filename[iglt],
			  glt_rows[iglt],
			  plug_p,
			  &(glt_cmat[iglt]));
	if (glt_cmat[iglt].elts == NULL)
	  { 
	    return "**************************\n"
                   "Unable to read GLT matrix \n"
	           "**************************"  ;
	  }
      } 


  /*--- nothing left to do until data arrives ---*/
  initialize = 1 ;  /* successful initialization */
  prev_nt = 0;      /* previous time series length */
  
  return NULL ;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the impulse response function and associated statistics.
*/

int calculate_results 
(
  int nt,               /* number of time points */
  double dt,            /* delta time */
  float * vec,          /* input measured data vector */
  int * nfirst,         /* first image from input 3d+time dataset to use */
  int * nlast,          /* last image from input 3d+time dataset to use */
  int * nfit,           /* number of fit parameters */
  float * fit,          /* fit parameters (regression coefficients) */
  char ** label         /* string containing statistical summary of results */
)
  
{
  float * ts_array;           /* array of measured data for one voxel */

  int N;                      /* number of data points */
  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the baseline model */
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
  int i;                 /* data point index */
  int is;                /* stimulus index */

  float rms_min = 0.0;   /* minimum variation in data to fit full model */
  int ok;                /* flag for successful matrix calculation */
  int novar;             /* flag for insufficient variation in data */
  float fglt[MAX_GLT];   /* F-statistics for the general linear tests */
  float rglt[MAX_GLT];   /* R^2 statistics for the general linear tests */


  /*----- Check initialization flag -----*/
  if (! initialize)  return (0);


  /*----- Initialize vectors -----*/
  vector_initialize (&coef);
  vector_initialize (&scoef);
  vector_initialize (&tcoef);
  vector_initialize (&y);
  

  /*----- Initialize local variables -----*/
  q = plug_polort + 1;
  p = plug_p;
  NFirst = plug_NFirst;
  NLast = plug_NLast;

  for (is = 0;  is < num_stimts;  is++)
    if (NFirst < max_lag[is])  NFirst = max_lag[is];
   
  if (NLast > nt-1)  NLast = nt-1;
  N = NLast - NFirst + 1;

  if (N <= p)  
    {
      DC_error ("Insufficient time series data for deconvolution fit");
      return (0);
    }

  *nfirst = NFirst;
  *nlast = NLast;
  *nfit = p;


  /*----- Perform initialization only if something has changed -----*/
  if (nt == prev_nt)
    {
      ok = 1;
    }
  else
    {
      /*----- Initialize the independent variable matrix -----*/
      ok = init_indep_var_matrix (p, q, NFirst, N, num_stimts,
				  stimulus, min_lag, max_lag, &xdata);

      
      /*----- Initialization for the regression analysis -----*/
      if (ok)
	ok = init_regression_analysis (p, q, num_stimts, min_lag, max_lag, 
			     xdata, &x_full, &xtxinv_full, &xtxinvxt_full,
			     &x_base, &xtxinvxt_base, x_rdcd, xtxinvxt_rdcd);


      /*----- Initialization for the general linear test analysis -----*/
      if (ok)
	if (glt_num > 0)
	  ok = init_glt_analysis (xtxinv_full, glt_num, glt_cmat, glt_amat);
    }
      
  if (ok)
    {
      /*----- Extract Y-data for this voxel -----*/
      vector_create (N, &y);
      ts_array = vec;
      for (i = 0;  i < N;  i++)
	y.elts[i] = ts_array[i+NFirst];
      
      
      /*----- Perform the regression analysis for this voxel-----*/
      regression_analysis (N, p, q, num_stimts, min_lag, max_lag,
			   x_full, xtxinv_full, xtxinvxt_full, x_base,
			   xtxinvxt_base, x_rdcd, xtxinvxt_rdcd, 
			   y, rms_min, &mse, &coef, &scoef, &tcoef, 
			   fpart, rpart, &ffull, &rfull, &novar);
      
 	  
      /*----- Perform the general linear tests for this voxel -----*/
      if (glt_num > 0)
	glt_analysis (N, p, x_full, y, mse*(N-p), coef, novar,
		  glt_num, glt_rows, glt_cmat, glt_amat, glt_coef, fglt, rglt);
      
     
      /*----- Save the fit parameters -----*/
      vector_to_array (coef, fit);
  
      
      /*----- Report results for this voxel -----*/
      printf ("\nResults for Voxel: \n");
      report_results (q, num_stimts, stim_label, min_lag, max_lag,
		      coef, tcoef, fpart, rpart, ffull, rfull, 
		      glt_num, glt_rows, glt_coef, fglt, rglt, label);
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

void DC_Fit (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  float val;               /* fitted value at a time point */ 
  int n;                   /* time index */
  int ifit;                /* parameter index */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int ok;                  /* Boolean for successful calculation */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &NFirst, &NLast, &nfit, fit, label);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Use the regression coefficients to calculate the fitted data -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      val = 0.0;
      for (ifit = 0;  ifit < nfit;  ifit++)  
	val += x_full.elts[n-NFirst][ifit] * fit[ifit];
      vec[n] = val;
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = vec[NFirst];

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = vec[NLast];
  
  
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the multiple linear regression residuals.
*/

void DC_Err (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  float val;               /* residual value at a time point */ 
  int n;                   /* time index */
  int ifit;                /* parameter index */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int ok;                  /* Boolean for successful calculation */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &NFirst, &NLast, &nfit, fit, label);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Use the regression coefficients to calculate the residuals -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      val = 0.0;
      for (ifit = 0;  ifit < nfit;  ifit++)  
	val += x_full.elts[n-NFirst][ifit] * fit[ifit];
      vec[n] = vec[n] - val;
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = 0.0;

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = 0.0;
  
  
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the system impulse response function.
*/
 
void DC_IRF (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int nfit;                /* number of fit parameters */
  float fit[MAX_XVARS];    /* fit parameters (regression coefficients) */
  int np;                  /* length of estimated impulse reponse function */
  int ip;                  /* impulse response function parameter index */
  int q;                   /* number of parameters in the baseline model */
  int it;                  /* array index */
  int ntdnp;               /* number of array points per IRF parameter */  
  int ok;                  /* Boolean for successful calculation */


  /*----- Calculate the multiple linear regression -----*/
  ok = calculate_results (nt, dt, vec, &NFirst, &NLast, &nfit, fit, label);
  if (!ok)
    {
      for (it = 0;  it < nt;  it++)  vec[it] = 0.0;
      return;
    }


  /*----- If IRF index is invalid, return all zeros -----*/
  if (num_stimts == 1)
    plug_IRF = 0;
  else
    if ((plug_IRF < 0) || (plug_IRF >= num_stimts))
      {
	for (it = 0;  it < nt;  it++)
	  vec[it] = 0.0;
	return;
      }


  /*----- Plot the system impulse response function -----*/
  np = max_lag[plug_IRF] - min_lag[plug_IRF] + 1;
  ntdnp = nt / np;

  q = plug_polort+1;
  for (ip = 0;  ip < plug_IRF;  ip++)
    q += max_lag[ip] - min_lag[ip] + 1;

  for (it = 0;  it < np*ntdnp;  it++)
    {
      ip = q + it/ntdnp;
      vec[it] = fit[ip];
    }

  for (it = np*ntdnp;  it < nt;  it++)
    vec[it] = 0.0;

  
  return;
}


/*---------------------------------------------------------------------------*/














