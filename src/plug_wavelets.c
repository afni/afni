/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Plugin to perform wavelet analysis of time series data.

  File:    plug_wavelet.c
  Author:  B. Douglas Ward
  Date:    28 March 2000
*/


/*---------------------------------------------------------------------------*/
/*
  Software identification.
*/

#define PROGRAM_NAME "plug_wavelets"                 /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "28 March 2000"             /* date of last program mod */


/*---------------------------------------------------------------------------*/
/*
  Global constants. 
*/

#define MAX_NAME_LENGTH 80              /* max. streng length for file names */
#define MAX_FILTERS 20                    /* max. number of blocking filters */
#define MAX_BAND 20                       /* max. frequency band */


/*---------------------------------------------------------------------------*/
/*
  Include header files and source code.
*/

#include "afni.h"
#include "Wavelets.h"
#include "Wavelets.c"


/*---------------------------------------------------------------------------*/

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif


/*------------- string to 'help' the user -------------*/

static char helpstring[] =
   " Purpose:    Wavelet Analysis of FMRI time series data.                 \n"
   "                                                                        \n"
   " Control:    Wavelet    = Type of wavelet to be used in analysis        \n"
   "             NFirst     = Number of first time series point to use.     \n"
   "             NLast      = Number of last time series point to use.      \n"
   "                                                                        \n"
   " Filter:     Type       = Type of filtering to apply in this            \n"
   "                          time-frequency window:                        \n"
   "               Stop     = set wavelet coefficients to zero              \n"
   "               Baseline = assign wavelet coefficients to baseline model \n"
   "               Signal   = assign wavelet coefficients to signal model   \n"
   "                                                                        \n"
   "             Band       = Frequency band at which to apply filtering.   \n"
   "             Min TR     = Minimum value for time window (in TR)         \n"
   "             Max TR     = Maximum value for time window (in TR)         \n"
   "                                                                        \n"
   "For more information, see 'Wavelet Analysis of FMRI Time Series Data'   \n"
   "which is contained in file 3dWavelets.ps of the AFNI distribution.      \n"
;


/*------- Strings for filter type ------*/
#define NFILTER 3
static char * filter_strings[NFILTER] = {"Stop",  "Baseline", "Signal"};


/*--------------- prototypes for internal routines ---------------*/

char * WA_main( PLUGIN_interface * ) ;  /* the entry point */

void WA_fwt  (int nt, double to, double dt, float * vec, char ** label);
void WA_fit  (int nt, double to, double dt, float * vec, char ** label);
void WA_sgnl (int nt, double to, double dt, float * vec, char ** label);
void WA_err  (int nt, double to, double dt, float * vec, char ** label);



/*---------------- global data -------------------*/

static PLUGIN_interface * global_plint = NULL;

static int plug_wavelet_type=0;        /* type of wavelet to use in analysis */
static int plug_NFirst=0;   /* first image from input 3d+time dataset to use */
static int plug_NLast=32767; /* last image from input 3d+time dataset to use */
static int plug_initialize=0;             /* flag for perform initialization */
static int plug_prev_nt=0;                    /* previous time series length */
static int plug_filter_type=0;          /* type of filter to use in analysis */

static int plug_num_stop_filters = 0;    /* number of stop filters */
static int plug_stop_band[MAX_FILTERS];  /* freq band for stop filter */
static int plug_stop_mintr[MAX_FILTERS]; /* min time for stop filter */
static int plug_stop_maxtr[MAX_FILTERS]; /* max time for stop filter */
static float * plug_stop_filter = NULL;  /* select wavelet coefs. to stop */

static int plug_num_base_filters = 0;    /* number of base filters */
static int plug_base_band[MAX_FILTERS];  /* freq band for base filter */
static int plug_base_mintr[MAX_FILTERS]; /* min time for base filter */
static int plug_base_maxtr[MAX_FILTERS]; /* max time for base filter */
static float * plug_base_filter = NULL;  /* select wavelet coefs. 
                                            for baseline */

static int plug_num_sgnl_filters = 0;    /* number of signal filters */
static int plug_sgnl_band[MAX_FILTERS];  /* freq band for signal filter */
static int plug_sgnl_mintr[MAX_FILTERS]; /* min time for signal filter */
static int plug_sgnl_maxtr[MAX_FILTERS]; /* max time for signal filter */
static float * plug_sgnl_filter = NULL;  /* select wavelet coefs. for signal */


/*---------------------------------------------------------------------------*/
/*
   Set up the interface to the user.
*/

PLUGIN_interface * PLUGIN_init( int ncall )
{
  PLUGIN_interface * plint ;     /* will be the output of this routine */
  int is;                        /* filter index */


  if( ncall > 0 ) return NULL ;  /* generate interface for ncall 0 */


  /***** do interface #0 *****/

  /*---------------- set titles and call point ----------------*/

  plint = PLUTO_new_interface ("Wavelets" ,
			       "Wavelet Analysis of Time Series Data" ,
			       helpstring, PLUGIN_CALL_VIA_MENU, WA_main);

  global_plint = plint ;  /* make global copy */
  
  PLUTO_add_hint (plint, 
		  "Control Wavelet Analysis Functions");

  PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

  
  /*----- Initialize Global Variables -----*/
  for (is =0;  is < MAX_FILTERS;  is++)
    {
      plug_stop_band[is]  = 0;
      plug_stop_mintr[is] = 0.0;
      plug_stop_maxtr[is] = 0.0;
      plug_base_band[is]  = 0;
      plug_base_mintr[is] = 0.0;
      plug_base_maxtr[is] = 0.0;
      plug_sgnl_band[is]  = 0;
      plug_sgnl_mintr[is] = 0.0;
      plug_sgnl_maxtr[is] = 0.0;
    }
   

  /*----- Parameters -----*/
  PLUTO_add_option (plint, "Control",   "Control", TRUE);
  PLUTO_add_string (plint, "Wavelet",   MAX_WAVELET_TYPE,  WAVELET_TYPE_name, 
		    plug_wavelet_type);
  PLUTO_add_number (plint, "NFirst",    0, 32767, 0, plug_NFirst, TRUE);
  PLUTO_add_number (plint, "NLast",     0, 32767, 0, plug_NLast,  TRUE);


  /*----- Stop Filters -----*/
  for (is = 0;  is < MAX_FILTERS;  is++)
    {
      PLUTO_add_option (plint, "Filter", "Filter", FALSE);
      PLUTO_add_string (plint, "Type",   NFILTER,  filter_strings, 
			plug_filter_type);
      PLUTO_add_number (plint, "Band",  -1, MAX_BAND, 0, 0, TRUE);
      PLUTO_add_number (plint, "Min TR", 0, 10000, 0, 0, TRUE);
      PLUTO_add_number (plint, "Max TR", 0, 10000, 0, 0, TRUE);
    }


  /*--------- done with interface setup ---------*/
  PLUTO_register_1D_funcstr ("WA_FWT",  WA_fwt);
  PLUTO_register_1D_funcstr ("WA_Fit",  WA_fit);
  PLUTO_register_1D_funcstr ("WA_Sgnl", WA_sgnl);
  PLUTO_register_1D_funcstr ("WA_Err",  WA_err);


  return plint ;
}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
*/

char * WA_main( PLUGIN_interface * plint )
{
  char * str;                           /* input string */
  int is;                               /* filter index */
  

  /*----- reset flag for successful initialization -----*/
  plug_initialize = 0;
    

  /*--------- go to Control input line ---------*/
  PLUTO_next_option (plint);
  str    = PLUTO_get_string (plint);
  plug_wavelet_type = PLUTO_string_index (str, MAX_WAVELET_TYPE, 
					  WAVELET_TYPE_name);
  plug_NFirst = PLUTO_get_number (plint);
  plug_NLast  = PLUTO_get_number (plint);


  /*------ read input line(s) -----*/
  plug_num_stop_filters = 0;
  plug_num_base_filters = 0;
  plug_num_sgnl_filters = 0;

  do
    {
      str = PLUTO_get_optiontag(plint); 
      if (str == NULL)  break;
      if (strcmp (str, "Filter") != 0)
        return "************************\n"
               "Illegal optiontag found!\n"
               "************************";
     

      /*----- Read Filter Specification Line -----*/
      str    = PLUTO_get_string (plint);
      plug_filter_type = PLUTO_string_index (str, NFILTER, filter_strings);
      
      switch (plug_filter_type)
	{
	case 0:
	  {
	    plug_stop_band[plug_num_stop_filters] = PLUTO_get_number(plint);   
	    plug_stop_mintr[plug_num_stop_filters] 
	      = PLUTO_get_number(plint);
	    plug_stop_maxtr[plug_num_stop_filters] 
	      = PLUTO_get_number(plint);
	  
	    if (plug_stop_mintr[plug_num_stop_filters] > 
		plug_stop_maxtr[plug_num_stop_filters])
	      return "*************************\n"
		"Require Min TR <= Max TR \n"
		"*************************"  ;
	    
	    plug_num_stop_filters++;
	    break;
	  }

	case 1:
	  {
	    plug_base_band[plug_num_base_filters] = PLUTO_get_number(plint);   
	    plug_base_mintr[plug_num_base_filters] 
	      = PLUTO_get_number(plint);
	    plug_base_maxtr[plug_num_base_filters] 
	      = PLUTO_get_number(plint);
	  
	    if (plug_base_mintr[plug_num_base_filters] > 
		plug_base_maxtr[plug_num_base_filters])
	      return "*************************\n"
		"Require Min TR <= Max TR \n"
		"*************************"  ;
	    
	    plug_num_base_filters++;
	    break;
	  }

	case 2:
	  {
	    plug_sgnl_band[plug_num_sgnl_filters]=PLUTO_get_number(plint); 
	    plug_sgnl_mintr[plug_num_sgnl_filters]
	      = PLUTO_get_number(plint);
	    plug_sgnl_maxtr[plug_num_sgnl_filters]
	      = PLUTO_get_number(plint);
	  
	    if (plug_sgnl_mintr[plug_num_sgnl_filters] > 
		plug_sgnl_maxtr[plug_num_sgnl_filters])
	      return "*************************\n"
		"Require Min TR <= Max TR \n"
		"*************************"  ;
	    
	    plug_num_sgnl_filters++;
	    break;
	  }

	}
    }
  while (1);


  /*----- show current input options -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Date:    %s \n", PROGRAM_DATE);

  printf ("\nControls: \n");
  printf ("Wavelet Type = %10s \n", WAVELET_TYPE_name[plug_wavelet_type]);
  printf ("NFirst       = %10d \n", plug_NFirst);
  printf ("NLast        = %10d \n", plug_NLast);

  for (is = 0;  is < plug_num_stop_filters;  is++)
    {
      printf ("\nStop Filter:       Band = %4d   ", plug_stop_band[is]);
      printf ("Min. TR = %4d   Max. TR = %4d \n", 
	      plug_stop_mintr[is], plug_stop_maxtr[is]);
    }
 
  for (is = 0;  is < plug_num_base_filters;  is++)
    {
      printf ("\nBaseline Filter:   Band = %4d   ", plug_base_band[is]);
      printf ("Min. TR = %4d   Max. TR = %4d \n", 
	      plug_base_mintr[is], plug_base_maxtr[is]);
    }
 
  for (is = 0;  is < plug_num_sgnl_filters;  is++)
    {
      printf ("\nSignal Filter:     Band = %4d   ", plug_sgnl_band[is]);
      printf ("Min. TR = %4d   Max. TR = %4d \n", 
	      plug_sgnl_mintr[is], plug_sgnl_maxtr[is]);
    }
 

  /*--- nothing left to do until data arrives ---*/
  plug_initialize = 1 ;  /* successful initialization */
  plug_prev_nt = 0;      /* previous time series length */
  
  return NULL ;
}


/*---------------------------------------------------------------------------*/
/*
  Perform the wavelet analysis.
*/

int calculate_results 
(
  int nt,               /* number of time points */
  float * vec,          /* input measured time series data */
  int * NFirst,         /* first image from input 3d+time dataset to use */
  int * NLast,          /* last image from input 3d+time dataset to use */
  char ** label,        /* string containing statistical summary of results */
  float ** coefts,      /* forward wavelet transform coefficients */
  float ** fitts,       /* full model fit to the time series data */
  float ** sgnlts,      /* signal model fit to the time series data */
  float ** errts        /* residual error time series */
)
  
{
  float * ts_array;        /* array of measured data for one voxel */
  float * coef;            /* regression parameters */
  float sse_base;          /* baseline model error sum of squares */
  float sse_full;          /* full model error sum of squares */
  float ffull;             /* full model F-statistic */
  float rfull;             /* full model R^2 stat. */

  int N;                   /* number of data points */
  int f;                   /* number of parameters removed by the filter */
  int p;                   /* number of parameters in the full model */
  int q;                   /* number of parameters in the baseline model */
  int n;                   /* data point index */
  int i;                   /* data point index */
  int ok = 1;


  /*----- Check initialization flag -----*/
  if (! plug_initialize)  return (0);


  /*----- Initialize local variables -----*/
  *NFirst = plug_NFirst;

  *NLast = plug_NLast;
  if (*NLast > nt-1)  *NLast = nt-1;

  N = *NLast - *NFirst + 1;
  N = powerof2(my_log2(N));
  *NLast = N + *NFirst - 1;


  plug_stop_filter = 
    FWT_1d_stop_filter (plug_num_stop_filters, plug_stop_band,
			plug_stop_mintr, plug_stop_maxtr, *NFirst, N);

  plug_base_filter = 
    FWT_1d_pass_filter (plug_num_base_filters, plug_base_band,
			plug_base_mintr, plug_base_maxtr, *NFirst, N);

  plug_sgnl_filter = 
    FWT_1d_pass_filter (plug_num_sgnl_filters, plug_sgnl_band,
			plug_sgnl_mintr, plug_sgnl_maxtr, *NFirst, N);

  f = 0;
  for (i = 0;  i < N;  i++)
    if (plug_stop_filter[i] == 0.0)
      {
	f++;
	plug_base_filter[i] = 0.0;
	plug_sgnl_filter[i] = 0.0;
      }

  q = 0;
  for (i = 0;  i < N;  i++)
    if (plug_base_filter[i] == 1.0)
      {
	q++;
	plug_sgnl_filter[i] = 1.0;
      }

  p = 0;
  for (i = 0;  i < N;  i++)
    if (plug_sgnl_filter[i] == 1.0)
      {
	p++;
      }    


  /*----- Allocate memory for fitted time series and residuals -----*/
   coef     = (float *) malloc (sizeof(float) * p);    MTEST (coef);
  *coefts   = (float *) malloc (sizeof(float) * N);    MTEST (coefts);
  *fitts    = (float *) malloc (sizeof(float) * N);    MTEST (fitts);
  *sgnlts   = (float *) malloc (sizeof(float) * N);    MTEST (sgnlts);
  *errts    = (float *) malloc (sizeof(float) * N);    MTEST (errts);


  /*----- Extract data for this voxel -----*/
  ts_array = vec + *NFirst;
      
      
  /*----- Perform the wavelet analysis for this voxel-----*/
  wavelet_analysis (plug_wavelet_type, 
		    f, plug_stop_filter,
		    q, plug_base_filter, 
		    p, plug_sgnl_filter,
		    N, ts_array, coef,
		    &sse_base, &sse_full, &ffull, &rfull,
		    *coefts, *fitts, *sgnlts, *errts);

      
  /*----- Report results for this voxel -----*/
  printf ("\nResults for Voxel: \n");
  report_results (N, *NFirst, f, p, q, 
		  plug_base_filter, plug_sgnl_filter, 
		  coef, sse_base, sse_full, ffull, rfull, 
		  label);  
  printf ("%s \n", *label);

  plug_prev_nt = nt;
   

  /*----- Release memory -----*/
  free (plug_stop_filter);   plug_stop_filter = NULL;
  free (plug_base_filter);   plug_base_filter = NULL;
  free (plug_sgnl_filter);   plug_sgnl_filter = NULL;
  free (coef);               coef     = NULL;


  return (ok);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the forward wavelet transform coefficients.
*/

void WA_fwt (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int n;                   /* time index */
  int ok;                  /* Boolean for successful calculation */
  float * coefts  = NULL;     /* forward wavelet transform coefficients */
  float * fitts   = NULL;     /* wavelet filtered time series */
  float * sgnlts  = NULL;     /* signal model fitted time series */
  float * errts   = NULL;     /* residual error time series */


  /*----- Calculate the wavelet filtered time series data -----*/
  ok = calculate_results (nt, vec, &NFirst, &NLast, label, 
			  &coefts, &fitts, &sgnlts, &errts);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Store the filtered time series data back into the array -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      vec[n] = coefts[n-NFirst];
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = 0.0;

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = 0.0;
    

  /*----- Deallocate memory -----*/
  free (coefts);   coefts = NULL;
  free (fitts);    fitts  = NULL;
  free (sgnlts);   sgnlts = NULL;
  free (errts);    errts  = NULL;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the full model wavelet fit to the time series data.
*/

void WA_fit (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int n;                   /* time index */
  int ok;                  /* Boolean for successful calculation */
  float * coefts  = NULL;     /* forward wavelet transform coefficients */
  float * fitts   = NULL;     /* wavelet filtered time series */
  float * sgnlts  = NULL;     /* signal model fitted time series */
  float * errts   = NULL;     /* residual error time series */


  /*----- Calculate the wavelet filtered time series data -----*/
  ok = calculate_results (nt, vec, &NFirst, &NLast, label, 
			  &coefts, &fitts, &sgnlts, &errts);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Store the filtered time series data back into the array -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      vec[n] = fitts[n-NFirst];
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = vec[NFirst];

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = vec[NLast];
    

  /*----- Deallocate memory -----*/
  free (coefts);   coefts = NULL;
  free (fitts);    fitts  = NULL;
  free (sgnlts);   sgnlts = NULL;
  free (errts);    errts  = NULL;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the signal model wavelet fit to the time series data.
*/

void WA_sgnl (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int n;                   /* time index */
  int ok;                  /* Boolean for successful calculation */
  float * coefts  = NULL;     /* forward wavelet transform coefficients */
  float * fitts   = NULL;     /* wavelet filtered time series */
  float * sgnlts  = NULL;     /* signal model fitted time series */
  float * errts   = NULL;     /* residual error time series */


  /*----- Calculate the wavelet filtered time series data -----*/
  ok = calculate_results (nt, vec, &NFirst, &NLast, label, 
			  &coefts, &fitts, &sgnlts, &errts);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Store the filtered time series residuals back into the array -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      vec[n] = sgnlts[n-NFirst];
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = 0.0;

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = 0.0;
    

  /*----- Deallocate memory -----*/
  free (coefts);   coefts = NULL;
  free (fitts);    fitts  = NULL;
  free (sgnlts);   sgnlts = NULL;
  free (errts);    errts  = NULL;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the residual error from the full model wavelet fit.
*/

void WA_err (int nt, double to, double dt, float * vec, char ** label)

{
  int NFirst;              /* first image from input 3d+time dataset to use */
  int NLast;               /* last image from input 3d+time dataset to use */
  int n;                   /* time index */
  int ok;                  /* Boolean for successful calculation */
  float * coefts  = NULL;     /* forward wavelet transform coefficients */
  float * fitts   = NULL;     /* wavelet filtered time series */
  float * sgnlts  = NULL;     /* signal model fitted time series */
  float * errts   = NULL;     /* residual error time series */


  /*----- Calculate the wavelet filtered time series data -----*/
  ok = calculate_results (nt, vec, &NFirst, &NLast, label, 
			  &coefts, &fitts, &sgnlts, &errts);
  if (!ok)
    {
      for (n = 0;  n < nt;  n++)  vec[n] = 0.0;
      return;
    }


  /*----- Store the filtered time series residuals back into the array -----*/
  for (n = NFirst;  n <= NLast;  n++)
    {
      vec[n] = errts[n-NFirst];
    }

  for (n = 0;  n < NFirst;  n++)
    vec[n] = 0.0;

  for (n = NLast+1;  n < nt;  n++)
    vec[n] = 0.0;
    

  /*----- Deallocate memory -----*/
  free (coefts);   coefts = NULL;
  free (fitts);    fitts  = NULL;
  free (sgnlts);   sgnlts = NULL;
  free (errts);    errts  = NULL;


  return;
}


/*---------------------------------------------------------------------------*/














