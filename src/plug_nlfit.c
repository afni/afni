/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   Plugin to calculate a nonlinear regression for an input time series, 
   using a specified nonlinear model.

   File:     plug_nlfit.c
   Author:   B. Douglas Ward
   Date:     17 June 1997

   Mod:      Added external time reference capability and added option for 
             absolute noise parameter constraints.
   Date:     22 August 1997

   Mod:      Added options for percent signal change above baseline, and
             calculation of area under the signal above baseline.
   Date:     26 November 1997

   Mod:      Print error message if unable to locate any signal models or 
             any noise models.
   Date:     26 December 1997

   Mod:      Added novar flag to eliminate unnecessary calculations.
   Date:     13 July 1999

   Mod:      Adjust F-statistics if parameter constraints force a parameter
             to be a constant.
   Date:     08 February 2000

   Mod:      Changes for output of R^2 (coefficient of multiple determination),
             and standard deviation of residuals from full model fit.
	     Added global variable calc_tstats.
             Also, added screen display of p-values.
   Date:     10 May 2000

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "plug_nlfit"                    /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "10 May 2000"               /* date of last program mod */

/*---------------------------------------------------------------------------*/


#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif


#include <math.h>
#include <stdlib.h>

#include "mrilib.h"

#include "matrix.h"
#include "simplex.h"
#include "NLfit.h"

#include "simplex.c"
#include "NLfit.c"

/***** 22 July 1998 -- RWCox:
       Modified to allow DELT to be set from the TR of the input file *****/

static float DELT = 1.0;   /* default */
static int   inTR = 0 ;    /* set to 1 if -inTR option is used */
static float dsTR = 0.0 ;  /* TR of the input file */


/***********************************************************************
  Plugin to provide nonlinear least squares fitting 1D function for graphs
************************************************************************/

/*------------- string to 'help' the user -------------*/

static char helpstring[] =
   " Purpose: Control the 'NLfit' and 'NLerr' 1D functions.\n"
   "\n"
   " Control:    Ignore       = Number of points to ignore at start \n"
   "                            of each timeseries. \n"
   "             NRandom      = Number of random test points. \n"
   "             NBest        = Find opt. soln. for these best test points. \n"
   " \n"
   " Models:     Noise        = Label of noise model to use. \n"
   "             Signal       = Label of signal model to use. \n"
   "             Noise Constr = Relative or Absolute noise constraints. \n"
   " \n"
   " Noise:      Parameter    = Index of noise model parameter. \n"
   "             Min Constr   = Minimum value for noise model parameter. \n"
   "             Max Constr   = Maximum value for noise model parameter. \n"
   " \n"
   " Signal:     Parameter    = Index of signal model parameter.\n"
   "             Min Constr   = Minimum value for signal model parameter. \n"
   "             Max Constr   = Maximum value for signal model parameter. \n"
   " \n"
   " Time Scale: Reference    = Internal or External time reference. \n"
   "             File         = External time reference file name. \n" 
   "Author -- BD Ward"
;



/*--------------- prototypes for internal routines ---------------*/

char * NL_main( PLUGIN_interface * ) ;  /* the entry point */

void NL_fitter( int nt, double to, double dt, float * vec, char ** label ) ;
void NL_error ( int nt, double to, double dt, float * vec, char ** label ) ;
void NL_worker( int nt, double dt, float * vec, int dofit, char ** label ) ;


/*---------------- global data -------------------*/
static PLUGIN_interface * global_plint = NULL ;
static int initialize=1 ;


/*------- Global data for models ------*/
char * constr_types[2] = {"Relative", "Absolute"};   /* option labels */
char * time_refs[3] = {"Internal", "External" , "-inTR" };
int plug_ignore = 3;
int plug_nrand = 100;
int plug_nbest = 5;
int plug_nabs = 0;
int plug_timeref = 0;
char plug_tfilename[MAX_NAME_LENGTH] = "";

/*----- declare reduced (noise) model variables -----*/
int num_noise_models;                /* number of noise models */
int plug_noise_index;                /* index of noise model */
char * noise_labels[MAX_MODELS];     /* names of noise models */
vfp plug_nmodel[MAX_MODELS];     /* pointer to noise model */
int plug_r[MAX_MODELS];          /* number of parameters in the noise model */
char * noise_plabels[MAX_MODELS][MAX_PARAMETERS];
float plug_min_nconstr[MAX_MODELS][MAX_PARAMETERS]; 
                           /* minimum parameter constraints for noise model */
float plug_max_nconstr[MAX_MODELS][MAX_PARAMETERS]; 
                           /* maximum parameter constraints for noise model */

/*----- declare full (signal+noise) model variables -----*/
int num_signal_models;               /* number of signal models */
int plug_signal_index;               /* index of signal model */
char * signal_labels[MAX_MODELS];    /* names of signal models */
vfp plug_smodel[MAX_MODELS];     /* pointer to signal model */
int plug_p[MAX_MODELS];          /* number of parameters in the signal model */
char * signal_plabels[MAX_MODELS][MAX_PARAMETERS];
float plug_min_sconstr[MAX_MODELS][MAX_PARAMETERS];  
                           /* minimum parameter constraints for signal model */
float plug_max_sconstr[MAX_MODELS][MAX_PARAMETERS];  
                           /* maximum parameter constraints for signal model */


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  int * im1,               /* index of 1st image in time series for analysis */
  char ** nname,           /* noise model name */
  char ** sname,           /* signal model name */
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  int * r,                 /* number of parameters in the noise model */
  int * p,                 /* number of parameters in the signal model */
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  int * nabs,              /* use absolute constraints for noise parameters */
  int * nrand,             /* number of random vectors to generate */
  int * nbest,             /* number of random vectors to keep */
  float * rms_min,         /* minimum rms error to reject reduced model */
  char ** tfilename        /* file name for time point series */  
)
 
{
  int ip;                           /* parameter index */
  int ok;                           /* boolean for specified model exists */
  char message[MAX_NAME_LENGTH];    /* error message */


  *im1 = 1;
  *nrand = plug_nrand;
  *nbest = plug_nbest; 
  *nabs = plug_nabs;
  *rms_min = 0.0;
  *tfilename = plug_tfilename;

  *nname = noise_labels[plug_noise_index];
  *sname = signal_labels[plug_signal_index];

  *nmodel = plug_nmodel[plug_noise_index];
  *smodel = plug_smodel[plug_signal_index];

  *r = plug_r[plug_noise_index];
  *p = plug_p[plug_signal_index];

  *npname = noise_plabels[plug_noise_index];
  *spname = signal_plabels[plug_signal_index];

  /*----- allocate memory for parameter constraints -----*/
  *min_nconstr = (float *) malloc (sizeof(float) * (*r));
  if (*min_nconstr == NULL)  
    NLfit_error ("Unable to allocate memory for min_nconstr");
  *max_nconstr = (float *) malloc (sizeof(float) * (*r));
  if (*max_nconstr == NULL)
    NLfit_error ("Unable to allocate memory for max_nconstr");
  *min_sconstr = (float *) malloc (sizeof(float) * (*p));
  if (*min_sconstr == NULL)  
    NLfit_error ("Unable to allocate memory for min_sconstr");
  *max_sconstr = (float *) malloc (sizeof(float) * (*p));
  if (*max_sconstr == NULL)
    NLfit_error ("Unable to allocate memory for max_sconstr");
  
  /*----- initialize constraints -----*/
  for (ip = 0;  ip < (*r);  ip++)
    {
      (*min_nconstr)[ip] = plug_min_nconstr[plug_noise_index][ip];
      (*max_nconstr)[ip] = plug_max_nconstr[plug_noise_index][ip];      
    }
  
  for (ip = 0;  ip < (*p);  ip++)
    {
      (*min_sconstr)[ip] = plug_min_sconstr[plug_signal_index][ip];
      (*max_sconstr)[ip] = plug_max_sconstr[plug_signal_index][ip];      
    }
 
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs ()
{
}


/*---------------------------------------------------------------------------*/

void initialize_program 
(
  int * im1,               /* index of 1st image in time series for analysis */
  char ** nname,           /* noise model name */
  char ** sname,           /* signal model name */
  vfp * nmodel,            /* pointer to noise model */
  vfp * smodel,            /* pointer to signal model */  
  int * r,                 /* number of parameters in the noise model */
  int * p,                 /* number of parameters in the signal model */
  char *** npname,         /* noise parameter names */
  char *** spname,         /* signal parameter names */
  float ** min_nconstr,    /* minimum parameter constraints for noise model */
  float ** max_nconstr,    /* maximum parameter constraints for noise model */
  float ** min_sconstr,    /* minimum parameter constraints for signal model */
  float ** max_sconstr,    /* maximum parameter constraints for signal model */
  int * nabs,              /* use absolute constraints for noise parameters */
  int * nrand,             /* number of random vectors to generate */
  int * nbest,             /* number of random vectors to keep */
  float * rms_min,         /* minimum rms error to reject reduced model */

  float ** par_rdcd,       /* estimated parameters for the reduced model */
  float ** par_full,       /* estimated parameters for the full model */
  float ** tpar_full,      /* t-statistic of parameters in the full model */

  int ts_length,           /* length of time series data */  
  char ** tfilename,       /* file name for time point series */  
  float *** x_array,       /* independent variable matrix */

  float ** fit
)

{
  int dimension;           /* dimension of full model */
  int ip;                  /* parameter index */
  int it;                  /* time index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
                              -- used to read 1D ASCII */
  int nt;                  /* number of points in 1D x data file */
  float * tar;


  /*----- intialize options -----*/
  initialize_options (im1, nname, sname, nmodel, smodel, r, p, npname, spname, 
		      min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		      nabs, nrand, nbest, rms_min, tfilename);

  /*----- check for valid inputs -----*/
  check_for_valid_inputs ();


  /*----- allocate space for independent variable matrix -----*/
  *x_array = (float **) malloc (sizeof(float *) * ts_length);
  if (*x_array == NULL)
    NLfit_error ("Unable to allocate memory for x_array");
  for (it = 0;  it < ts_length;  it++)
    {
      (*x_array)[it] = (float *) malloc (sizeof(float) * 3);
      if ((*x_array)[it] == NULL)
	NLfit_error ("Unable to allocate memory for x_array[it]");
    }
    
  /*----- initialize independent variable matrix -----*/
  if (!plug_timeref)
    {
      static float old_DELT = -1.0 ;
      DELT = (inTR && dsTR > 0.0) ? dsTR : 1.0 ;  /* 22 July 1998 */
      if( DELT != old_DELT ){
         old_DELT = DELT ;
         printf("NLfit: switch to TR = %g\n",DELT) ;
      }

      for (it = 0;  it < ts_length;  it++)  
	{
	  (*x_array)[it][0] = 1.0;
	  (*x_array)[it][1] = it * DELT;
	  (*x_array)[it][2] = (it * DELT) * (it * DELT);
	}
    }
  else 
    {
        flim = mri_read_1D (*tfilename); 
	if (flim == NULL)
	  NLfit_error ("Unable to read time reference file \n");
        nt = flim -> nx;
	if (nt < ts_length)
	    NLfit_error ("Time reference array is too short");  
        tar = MRI_FLOAT_PTR(flim) ;
        for (it = 0;  it < ts_length;  it++)  
         {
	   (*x_array)[it][0] = 1.0;
           (*x_array)[it][1] = tar[it] ;
           (*x_array)[it][2] = tar[it] * tar[it];
         }
        mri_free (flim);
     }

  /*--- 24 Jul 2006: special change to x_array[][2] for Linear+Ort [RWCox] ---*/
   if( strcmp(*nname,"Linear+Ort") == 0 ){
      char *fname=NULL; MRI_IMAGE *fim=NULL; int nx; float *far; static int nwarn=0;
      fname = my_getenv("AFNI_ORTMODEL_REF") ;
      if( fname == NULL ){
        ERROR_message("Linear+Ort model: 'AFNI_ORTMODEL_REF' not set") ;
        goto PLO_done ;
      }
    
      fim = mri_read_1D(fname) ;
      if( fim == NULL || fim->nx < 2 ){
        ERROR_message(
          "Linear+Ort model: can't read AFNI_ORTMODEL_REF='%s'",fname) ;
        goto PLO_done ;
      }

      if( fim->ny > 1 && nwarn < 2 ){
        WARNING_message(
          "Linear+Ort model: file AFNI_ORTMODEL_REF='%s' has more than 1 column",
          fname ) ;
        nwarn++ ;
      }

      nx = fim->nx ; far = MRI_FLOAT_PTR(fim) ;
      if( nx != ts_length && nwarn ){
        WARNING_message("Linear+Ort: length(%s)=%d but length(dataset)=%d",
                        fname , nx , ts_length ) ;
        nwarn++ ;
      }
      for( it=0 ; it < ts_length;  it++)
        (*x_array)[it][2] = (it < nx) ? far[it] : 0.0f ;

     PLO_done: ; /* nada */
   }

  
  dimension = (*r) + (*p);

  /*----- allocate memory space -----*/
  *par_rdcd = (float *) malloc (sizeof(float) * dimension);
  if (*par_rdcd == NULL)
    NLfit_error ("Unable to allocate memory for par_rdcd");
  *par_full = (float *) malloc (sizeof(float) * dimension);
  if (*par_full == NULL)
    NLfit_error ("Unable to allocate memory for par_full");
  *tpar_full = (float *) malloc (sizeof(float) * dimension);
  if (*tpar_full == NULL)
    NLfit_error ("Unable to allocate memory for tpar_full");
  *fit = (float *) malloc (sizeof(float) * (ts_length));
  if (*fit == NULL)
    NLfit_error ("Unable to allocate memory for fit");

}


/*---------------------------------------------------------------------------*/
/*
  Release all allocated memory space.
*/

void terminate_program 
(
  int r,                       /* number of parameters in the noise model */
  int p,                       /* number of parameters in the signal model */
  int ts_length,               /* length of time series data */
  float *** x_array,           /* independent variable matrix */
  float ** par_rdcd,      /* estimated parameters for the reduced model */
  float ** min_nconstr,   /* min parameter constraints for noise model */
  float ** max_nconstr,   /* max parameter constraints for noise model */
  float ** par_full,      /* estimated parameters for the full model */
  float ** tpar_full,     /* t-statistic of parameters in full model */
  float ** min_sconstr,   /* min parameter constraints for signal model */
  float ** max_sconstr    /* max parameter constraints for signal model */
)
 
{
  int ip;                        /* parameter index */
  int it;                        /* time index */


  /*----- deallocate memory for parameter constraints -----*/
  if (*min_nconstr != NULL)  { free (*min_nconstr);  *min_nconstr = NULL; }
  if (*max_nconstr != NULL)  { free (*max_nconstr);  *max_nconstr = NULL; }
  if (*min_sconstr != NULL)  { free (*min_sconstr);  *min_sconstr = NULL; }
  if (*max_sconstr != NULL)  { free (*max_sconstr);  *max_sconstr = NULL; }


  /*----- deallocate space for independent variable matrix -----*/
  for (it = 0;  it < ts_length;  it++)
    if ((*x_array)[it] != NULL)
      { free ((*x_array)[it]);  (*x_array)[it] = NULL; }
  if (*x_array != NULL)     { free (*x_array);  *x_array = NULL; }


  /*----- deallocate space for parameters -----*/
  if (*par_rdcd != NULL)    { free (*par_rdcd);    *par_rdcd = NULL; }
  if (*par_full != NULL)    { free (*par_full);    *par_full = NULL; }
  if (*tpar_full != NULL)   { free (*tpar_full);   *tpar_full = NULL; }

}


/*---------------------------------------------------------------------------*/

float *  nlfit
(
  int ts_length,                     /* length of time series data */
  float * ts_array,                  /* input time series array */
  char ** label                      /* label output for this voxel */
)

{
  float * fit;                       /* nonlinear fit of time series data */

  /*----- declare input option variables -----*/
  int nabs;                /* use absolute constraints for noise parameters */
  int  nrand;              /* number of random vectors to generate */
  int  nbest;              /* number of random vectors to keep */
  float rms_min;           /* minimum rms error to reject reduced model */

  /*----- declare time series variables -----*/
  int im1;                 /* index of 1st image in time series for analysis */
  float ** x_array = NULL;     /* independent variable matrix */
  char * tfilename = NULL;     /* file name of time points */

  /*----- declare reduced (noise) model variables -----*/
  char * nname = NULL;         /* noise model name */
  vfp nmodel;                  /* pointer to noise model */
  int r;                       /* number of parameters in the noise model */
  char ** npname = NULL;       /* noise parameter labels */
  float * par_rdcd = NULL;     /* estimated parameters for the reduced model */
  float sse_rdcd;              /* error sum of squares for the reduced model */
  float * min_nconstr = NULL;  /* min parameter constraints for noise model */
  float * max_nconstr = NULL;  /* max parameter constraints for noise model */

  /*----- declare full (signal+noise) model variables -----*/
  char * sname = NULL;         /* signal model name */
  vfp smodel;                  /* pointer to signal model */
  int p;                       /* number of parameters in the signal model */
  char ** spname = NULL;       /* signal parameter labels */
  float * par_full = NULL;     /* estimated parameters for the full model */
  float sse_full;              /* error sum of squares for the full model */
  float * tpar_full = NULL;    /* t-statistic of parameters in full model */
  float freg;                  /* f-statistic for the full regression model */
  float rmsreg;                /* rms for the full regression model */
  float rsqr;                  /* R^2 (coef. of multiple determination) */
  float smax;                  /* signed maximum of signal */
  float tmax;                  /* epoch of signed maximum of signal */
  float pmax;                  /* percentage change due to signal */
  float area;                  /* area between signal and baseline */
  float parea;                 /* percent area between signal and baseline */
  float * min_sconstr = NULL;  /* min parameter constraints for signal model */
  float * max_sconstr = NULL;  /* max parameter constraints for signal model */

  int novar;               /* flag for insufficient variation in the data */

   
  /*----- program initialization -----*/
  initialize_program (&im1, &nname, &sname, &nmodel, &smodel, 
		      &r, &p, &npname, &spname,
		      &min_nconstr, &max_nconstr, &min_sconstr, &max_sconstr,
		      &nabs, &nrand, &nbest, &rms_min, 
		      &par_rdcd, &par_full, &tpar_full, 
		      ts_length, &tfilename, &x_array, &fit);
  

  /*----- calculate the reduced (noise) model -----*/  
  calc_reduced_model (ts_length, r, x_array, ts_array, 
		      par_rdcd, &sse_rdcd);
      

  /*----- calculate the full (signal+noise) model -----*/
  calc_full_model (nmodel, smodel, r, p, 
		   min_nconstr, max_nconstr, min_sconstr, max_sconstr,
		   ts_length, x_array, ts_array, par_rdcd, sse_rdcd, 
		   nabs, nrand, nbest, rms_min, par_full, &sse_full, &novar);


  /*----- create estimated time series using the full model parameters -----*/
  full_model (nmodel, smodel, par_full, par_full + r, 
	      ts_length, x_array, fit);
      

  /*----- calculate statistics for the full model -----*/
  analyze_results (nmodel, smodel, r, p, novar,
		   min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		   ts_length, x_array,
		   par_rdcd, sse_rdcd, par_full, sse_full,
		   &rmsreg, &freg, &rsqr, &smax, &tmax, &pmax, &area, &parea,
		   tpar_full);


  /*----- report results for this voxel -----*/
  report_results (nname, sname, r, p, npname, spname, ts_length,
		  par_rdcd, sse_rdcd, par_full, sse_full, tpar_full,
		  rmsreg, freg, rsqr, smax, tmax, pmax, area, parea, label);
  printf ("\nVoxel Results: \n");
  printf ("%s \n", *label);
  

  /*----- end of program -----*/
  terminate_program (r, p, ts_length, &x_array,
		     &par_rdcd, &min_nconstr, &max_nconstr, 
		     &par_full, &tpar_full, 
		     &min_sconstr, &max_sconstr); 

  return (fit);
  
}


/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset"    for a dataset chooser,
        "PLUTO_add_string"     for a string chooser,
        "PLUTO_add_number"     for a number chooser,
        "PLUTO_add_timeseries" for a timeseries chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   int ii ;
   PLUGIN_interface * plint ;     /* will be the output of this routine */
   int ok;

   NLFIT_MODEL_array * model_array = NULL;   /* array of SO models */
   int im;                                   /* model index */
   int ip;                                   /* parameter index */

   char message[MAX_NAME_LENGTH];    /* error message */


   if( ncall > 0 ) return NULL ;  /* generate interfaces for ncall 0 */

   jump_on_NLfit_error = 1 ;                 /* 01 May 2003: */
   if( setjmp(NLfit_error_jmpbuf) != 0 ){    /* NLfit_error() was invoked */
     jump_on_NLfit_error = 0 ;               /* somewhere below here */
     fprintf(stderr,"\n*** Can't load NLfit plugin! ***\n");
     return NULL ;
   }

   /***** otherwise, do interface # 0 *****/

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "NLfit & NLerr" ,
                                "Control NLfit and NLerr Functions" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , NL_main ) ;

   { char *eee = getenv("AFNI_NLFIM_METHOD") , str[94] ;
     if( eee == NULL || strcasecmp(eee,"simplex") == 0 )
       N_newuoa = 0 ;
     else if( strcasecmp(eee,"powell") == 0 )
       N_newuoa = 1 ;
     else if( strcasecmp(eee,"both") == 0 )
       N_newuoa = 2 ;
     else
       N_newuoa = 0 ;

     sprintf(str,"Optimizer (AFNI_NLFIM_METHOD) is %s" ,
             (N_newuoa==0) ? "SIMPLEX"
            :(N_newuoa==1) ? "POWELL" : "BOTH (SIMPLEX+POWELL)" ) ;
     PLUTO_report(plint,str) ;
   }

   PLUTO_add_hint( plint , "Control NLfit and NLerr Functions" ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_set_sequence( plint , "A:funcs:fitting" ) ;

   PLUTO_set_runlabels( plint , "Set+Keep" , "Set+Close" ) ;  /* 04 Nov 2003 */

   /*----- initialize the model array -----*/
   model_array = NLFIT_get_many_MODELs ();
   if ((model_array == NULL) || (model_array->num == 0))
#if 1
     { PLUTO_report( plint , "Found no models!\n") ;
       jump_on_NLfit_error = 0 ; return NULL ; }
#else
     NLfit_error ("Unable to locate any models");
#endif

#if 1
   else
   { char str[64] ;
     sprintf(str,"Found %d models\n",model_array->num) ;
     PLUTO_report( plint , str ) ;
   }
#endif

   /*----- read parameters for noise models -----*/
   ii = 0;
   for (im = 0;  im < model_array->num;  im++)
     {
       if (model_array->modar[im]->interface->model_type == MODEL_NOISE_TYPE)
	 {
	   noise_labels[ii] = (char *) malloc (sizeof(char)*MAX_NAME_LENGTH);
	   strncpy (noise_labels[ii], model_array->modar[im]->interface->label,
		    MAX_NAME_LENGTH);

	   plug_nmodel[ii] = model_array->modar[im]->interface->call_func;
	   if (plug_nmodel[ii] == NULL)
	    {
	      sprintf (message, "Noise model %s improperly defined. \n", 
		       noise_labels[ii]);
	      NLfit_error (message);
	    }
		  
	   plug_r[ii] = model_array->modar[im]->interface->params;
	   if ((plug_r[ii] < 0) || (plug_r[ii] > MAX_PARAMETERS))
	    {
	      sprintf (message, 
		       "Illegal number of parameters for noise model %s", 
		       noise_labels[ii]);
	      NLfit_error (message);
	    }
     	  
	   for (ip = 0;  ip < plug_r[ii];  ip++)
	     {
	       noise_plabels[ii][ip] = 
		 (char *) malloc (sizeof(char)*MAX_NAME_LENGTH);
	       strncpy (noise_plabels[ii][ip], 
		       model_array->modar[im]->interface->plabel[ip],
			MAX_NAME_LENGTH);
	       plug_min_nconstr[ii][ip] =
		 model_array->modar[im]->interface->min_constr[ip]; 
	       plug_max_nconstr[ii][ip] = 
		 model_array->modar[im]->interface->max_constr[ip]; 
	       if (plug_min_nconstr[ii][ip] > plug_max_nconstr[ii][ip])
		 NLfit_error
		   ("Must have noise parameter min cnstrnts <= max cnstrnts");
	     }
	   ii += 1;
	 }
     }
   num_noise_models = ii;
   if (num_noise_models <= 0)  
     NLfit_error ("Unable to locate any noise models");
   plug_noise_index = 1;


   /*----- read parameters for signal models -----*/
   ii = 0;
   for (im = 0;  im < model_array->num;  im++)
     {
       if (model_array->modar[im]->interface->model_type == MODEL_SIGNAL_TYPE)
	 {
	   signal_labels[ii] = (char *) malloc (sizeof(char)*MAX_NAME_LENGTH);
	   strncpy (signal_labels[ii],
		    model_array->modar[im]->interface->label,
		    MAX_NAME_LENGTH);

	   plug_smodel[ii] = model_array->modar[im]->interface->call_func;
	   if (plug_smodel[ii] == NULL)
	    {
	      sprintf (message, "Signal model %s improperly defined. \n", 
		       signal_labels[ii]);
	      NLfit_error (message);
	    }
		  
	   plug_p[ii] = model_array->modar[im]->interface->params;
	   if ((plug_p[ii] < 0) || (plug_p[ii] > MAX_PARAMETERS))
	    {
	      sprintf (message, 
		       "Illegal number of parameters for signal model %s", 
		       signal_labels[ii]);
	      NLfit_error (message);
	    }
     	  

	   for (ip = 0;  ip < plug_p[ii];  ip++)
	     {
	       signal_plabels[ii][ip] = 
		 (char *) malloc (sizeof(char)*MAX_NAME_LENGTH);
	       strncpy (signal_plabels[ii][ip], 
			model_array->modar[im]->interface->plabel[ip],
			MAX_NAME_LENGTH);
	       plug_min_sconstr[ii][ip] =
		 model_array->modar[im]->interface->min_constr[ip]; 
	       plug_max_sconstr[ii][ip] = 
		 model_array->modar[im]->interface->max_constr[ip]; 
	       if (plug_min_sconstr[ii][ip] > plug_max_sconstr[ii][ip])
		 NLfit_error
		   ("Must have signal parameter min cnstrnts <= max cnstrnts");
	     }
	   ii += 1;
	 }
     }
   num_signal_models = ii;
   if (num_signal_models <= 0)  
     NLfit_error ("Unable to locate any signal models");
   plug_signal_index = 0;


   /*----- Control -----*/
   PLUTO_add_option (plint , "Control" , "Control" , TRUE);
   PLUTO_add_number (plint , "Ignore" , 0,20,0,
		     plug_ignore , FALSE );
   PLUTO_add_number (plint , "NRandom" , 10,99999,0,
		     plug_nrand , TRUE );
   PLUTO_add_number (plint , "NBest" , 1,10,0,
		     plug_nbest , FALSE);


   /*----- Models -----*/
   PLUTO_add_option (plint , "Models" , "Models" , TRUE);
   PLUTO_add_string (plint, "Noise Model", num_noise_models, noise_labels, 
		     plug_noise_index);
   PLUTO_add_string (plint, "Signal Model", num_signal_models, signal_labels,
		     plug_signal_index);
   PLUTO_add_string (plint, "Noise Constr", 2, constr_types, 0);


   /*----- Noise Model Parameters -----*/
   PLUTO_add_option (plint, "Noise", "Noise", FALSE);
   PLUTO_add_number (plint, "Parameter" , 0, MAX_PARAMETERS, 0, 0, FALSE);
   PLUTO_add_number (plint, "Min Constr", -99999, 99999, 0, 0, TRUE);
   PLUTO_add_number (plint, "Max Constr", -99999, 99999, 0, 0, TRUE);


   /*----- Signal Model Parameters -----*/
   PLUTO_add_option (plint, "Signal", "Signal", FALSE);
   PLUTO_add_number (plint, "Parameter", 0, MAX_PARAMETERS, 0, 0, FALSE);
   PLUTO_add_number (plint, "Min Constr", -99999, 99999, 0, 0, TRUE);
   PLUTO_add_number (plint, "Max Constr", -99999, 99999, 0, 0, TRUE);


   /*----- External Time Reference -----*/
   PLUTO_add_option (plint, "Time Scale", "Time Scale", FALSE);
   PLUTO_add_string (plint, "Reference", 3, time_refs, 0);
   PLUTO_add_string (plint, "File", 0, NULL, 19);


   /*--------- done with interface setup ---------*/

   PLUTO_register_1D_funcstr ("NLfit" , NL_fitter);
   PLUTO_register_1D_funcstr ("NLerr" , NL_error);


   /*----- discard the model array -----*/
#if 0
   DESTROY_MODEL_ARRAY (model_array);
#endif

   jump_on_NLfit_error = 0 ; return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * NL_main( PLUGIN_interface * plint )
{
   char * str ;
   int  ii, ival, ip ;
   float * tsar ;
   float min_constr, max_constr;
   MRI_IMAGE * im;  /* pointer to image structures */


   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   plug_ignore = PLUTO_get_number(plint) ;
   plug_nrand = PLUTO_get_number(plint) ;
   plug_nbest = PLUTO_get_number(plint) ;


   /*------ loop over remaining options, check their tags, process them -----*/

   do 
     {
       str = PLUTO_get_optiontag(plint) ; 
       if( str == NULL ) break ;

       if( strcmp(str,"Models") == 0 )
	 {
	   str = PLUTO_get_string(plint) ;
	   for (ii = 0;  ii < num_noise_models;  ii++)
	     if (strcmp (str, noise_labels[ii]) == 0)
	       plug_noise_index = ii;
	 
	   str = PLUTO_get_string(plint) ;
	   for (ii = 0;  ii < num_signal_models;  ii++)
	     if (strcmp (str, signal_labels[ii]) == 0)
	       plug_signal_index = ii;

	   str = PLUTO_get_string(plint);
	   if (strcmp (str, "Absolute") == 0)
	     plug_nabs = 1;
	   else
	     plug_nabs = 0;
	 } 

       else if( strcmp(str,"Noise") == 0 )
	 {
	   ival = PLUTO_get_number(plint);
	   min_constr = PLUTO_get_number(plint);
	   max_constr = PLUTO_get_number(plint);
	   if (min_constr > max_constr)
	     return "**********************************\n"
	            " Require min constr <= max constr \n"
	            "**********************************"  ;
	   plug_min_nconstr[plug_noise_index][ival] = min_constr;
	   plug_max_nconstr[plug_noise_index][ival] = max_constr;
	 } 

       else if( strcmp(str,"Signal") == 0 )
	 {
	   ival = PLUTO_get_number(plint);
	   min_constr = PLUTO_get_number(plint);
	   max_constr = PLUTO_get_number(plint);
	   if (min_constr > max_constr)
	     return "**********************************\n"
	            " Require min constr <= max constr \n"
	            "**********************************"  ;
	   plug_min_sconstr[plug_signal_index][ival] = min_constr;
	   plug_max_sconstr[plug_signal_index][ival] = max_constr;
	 } 

       else if( strcmp(str,"Time Scale") == 0 )
	 {
	   str = PLUTO_get_string(plint);
	   if (strcmp (str, "External") == 0){
	       plug_timeref = 1;
	       str = PLUTO_get_string(plint);
	       im = mri_read_1D (str); 
	       if (im == NULL)
		 return "************************************\n"
		        " Unable to read time reference file \n"
	                "************************************"  ;
	       mri_free(im);
	       strcpy (plug_tfilename, str);

	   } else if( strcmp(str,"-inTR") == 0 ){  /* 22 July 1998 */
              inTR = 1 ;
	      plug_timeref = 0;

           } else {
	     plug_timeref = 0;
             inTR = 0 ;                       /* 22 July 1998 */
           }

	 } 

       else 
	 {
	   return "************************\n"
	          "Illegal optiontag found!\n"
	          "************************"  ;
	 }
     } while(1) ;


  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

   
   /*----- show current input options -----*/
   printf ("\nControls: \n");
   printf ("Ignore       = %5d \n", plug_ignore);
   printf ("Num Random   = %5d \n", plug_nrand);
   printf ("Num Best     = %5d \n", plug_nbest);
   printf ("Noise Constr = %s  \n", constr_types[plug_nabs]);
   printf ("\nNoise  Model = %s \n", noise_labels[plug_noise_index]);
   for (ip = 0;  ip < plug_r[plug_noise_index];  ip++)
     {
       printf ("gn[%d]:   min =%10.3f   max =%10.3f   %s \n", 
	       ip, plug_min_nconstr[plug_noise_index][ip], 
	       plug_max_nconstr[plug_noise_index][ip],
	       noise_plabels[plug_noise_index][ip]);
     }
   printf ("\nSignal Model = %s \n", signal_labels[plug_signal_index]);
   for (ip = 0;  ip < plug_p[plug_signal_index];  ip++)
     {
       printf ("gs[%d]:   min =%10.3f   max =%10.3f   %s \n", 
	       ip, plug_min_sconstr[plug_signal_index][ip], 
	       plug_max_sconstr[plug_signal_index][ip],
	       signal_plabels[plug_signal_index][ip]);
     }

   if (plug_timeref)
     printf ("\nExternal Time Reference = %s \n", plug_tfilename);
   else if( inTR )
     printf ("\n-inTR Time Reference\n") ;
   else
     printf ("\nInternal Time Reference \n");
   
   
   /*--- nothing left to do until data arrives ---*/
   
   initialize = 1 ;  /* force re-initialization */
   
   
   return NULL ;
}


/*---------------------------------------------------------------------------*/

void NL_fitter( int nt , double to , double dt , float * vec, char ** label )
{
   NL_worker( nt , dt , vec , TRUE, label ) ;
   return ;
}


/*---------------------------------------------------------------------------*/

void NL_error( int nt , double to , double dt , float * vec, char ** label )
{
   NL_worker( nt , dt , vec , FALSE, label ) ;
   return ;
}
 

/*---------------------------------------------------------------------------*/

void NL_worker( int nt , double dt , float * vec , int dofit, char ** label )
{
   float * fit;
   int ii, nlen;
   float val;


   nlen = nt - plug_ignore;

   dsTR = dt ;

   /** find least squares fit coefficients **/

   fit = nlfit (nlen, vec+plug_ignore, label);

   for (ii = 0;  ii < plug_ignore;  ii++)
     if (dofit)
       vec[ii] = fit[0];
     else
       vec[ii] = vec[plug_ignore] - fit[0];

   for (ii=plug_ignore; ii < nt; ii++)
     {
       if (dofit)
	 vec[ii] = fit[ii-plug_ignore];
       else
	 vec[ii] = vec[ii] - fit[ii-plug_ignore] ;
     }

   free(fit) ;
   return ;
}











