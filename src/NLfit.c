/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#define UNROLL

/*
  This file contains routines used by programs 3dNLfim, plug_nlfit, and
  3dTSgen for performing non-linear regression analysis of AFNI 3d+time
  data seta, and investigating various statistical properties of the data.

  File:     NLfit.c
  Author:   B. Douglas Ward
  Date:     19 June 1997

  Mod:      Added option for absolute noise parameter constraints.
  Date:     21 August 1997

  Mod:      Changed random number initialization from srand to srand48.
  Date:     29 August 1997

  Mod:      Added options for percent signal change above baseline, and
            calculation of area under the signal above baseline.
  Date:     26 November 1997

  Mod:      Continuation of previous modification.
  Date:     8 January 1998

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

  Mod:      Add stuff for longjmp() return from NLfit_error().
  Date:     01 May 2003 - RWCox

  Mod:      Add UNROLL to a couple loops that are executed a lot.
  Mod:      20 Jul 2006 - RWCox

  Mod:      Special initialization for new Linear+Ort noise model.
  Mod:      24 Jul 2006 - RWCox

*/

/*---------------------------------------------------------------------------*/

#include "NLfit_model.c"

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

#include <setjmp.h>                    /* 01 May 2003 */
static int jump_on_NLfit_error = 0 ;
static jmp_buf NLfit_error_jmpbuf ;

void NLfit_error
(
  char * message         /* message to be displayed */
)

{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   if( jump_on_NLfit_error ) longjmp( NLfit_error_jmpbuf , 1 ) ;  /* 01 May 2003 */
   exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the signal model by defining the number of parameters
  in the signal model, the default values for the minimum and maximum
  parameter constraints, and setting the pointer to the function which
  implements the signal model.
*/

void initialize_signal_model
(
  NLFIT_MODEL_array * model_array,       /* the array of SO models */
  char * sname,            /* name of signal model (user input) */
  vfp * smodel,            /* pointer to signal model */
  int * p,                 /* number of parameters in the signal model */
  char ** spname,          /* signal parameter names */
  float * min_sconstr,     /* minimum parameter constraints for signal model */
  float * max_sconstr      /* maximum parameter constraints for signal model */
)

{
  int im, ip, index;
  char message[MAX_NAME_LENGTH];    /* error message */


  index = -1;
  for (im = 0;  im < model_array->num;  im++)
    if (strncmp(model_array->modar[im]->interface->label,
                sname, MAX_NAME_LENGTH) == 0)  index = im;
  if (index < 0)
    {
      sprintf (message, "Unable to locate signal model %s", sname);
      NLfit_error (message);
    }

  if (model_array->modar[index]->interface->model_type != MODEL_SIGNAL_TYPE)
    {
      sprintf (message, "%s has not been declared a signal model", sname);
      NLfit_error (message);
    }

  *smodel = model_array->modar[index]->interface->call_func;
  if (*smodel == NULL)
    {
      sprintf (message, "Signal model %s not properly implemented", sname);
      NLfit_error (message);
    }

  *p = model_array->modar[index]->interface->params;
  if ((*p < 0) || (*p > MAX_PARAMETERS))
    {
      sprintf (message, "Illegal number of parameters for signal model %s",
               sname);
      NLfit_error (message);
    }

  for (ip = 0;  ip < *p;  ip++)
    {
      strncpy (spname[ip], model_array->modar[index]->interface->plabel[ip],
               MAX_NAME_LENGTH);
      min_sconstr[ip] = model_array->modar[index]->interface->min_constr[ip];
      max_sconstr[ip] = model_array->modar[index]->interface->max_constr[ip];
      if (min_sconstr[ip] > max_sconstr[ip])
      NLfit_error("Must have signal parameter min cnstrnts <= max cnstrnts");
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the noise model by defining the number of parameters
  in the noise model, the default values for the minimum and maximum
  parameter constraints, and setting the pointer to the function which
  implements the noise model.
*/

void initialize_noise_model
(
  NLFIT_MODEL_array * model_array,       /* the array of SO models */
  char * nname,            /* name of noise model (user input) */
  vfp * nmodel,            /* pointer to noise model */
  int * r,                 /* number of parameters in the noise model */
  char ** npname,          /* noise parameter names */
  float * min_nconstr,     /* minimum parameter constraints for noise model */
  float * max_nconstr      /* maximum parameter constraints for noise model */
)

{
  int im, ip, index;
  char message[MAX_NAME_LENGTH];    /* error message */


  index = -1;
  for (im = 0;  im < model_array->num;  im++)
    if (strncmp(model_array->modar[im]->interface->label,
                nname, MAX_NAME_LENGTH) == 0)  index = im;
  if (index < 0)
    {
      sprintf (message, "Unable to locate noise model %s", nname);
      NLfit_error (message);
    }

  if (model_array->modar[index]->interface->model_type != MODEL_NOISE_TYPE)
    {
      printf ("type = %d \n",
              model_array->modar[index]->interface->model_type);
      sprintf (message, "%s has not been declared a noise model", nname);
      NLfit_error (message);
    }

  *nmodel = model_array->modar[index]->interface->call_func;
  if (*nmodel == NULL)
    {
      sprintf (message, "Noise model %s not properly implemented", nname);
      NLfit_error (message);
    }

  *r = model_array->modar[index]->interface->params;
  if ((*r < 0) || (*r > MAX_PARAMETERS))
    {
      sprintf (message, "Illegal number of parameters for noise model %s",
               nname);
      NLfit_error (message);
    }

  for (ip = 0;  ip < *r;  ip++)
    {
      strncpy (npname[ip], model_array->modar[index]->interface->plabel[ip],
               MAX_NAME_LENGTH);
      min_nconstr[ip] = model_array->modar[index]->interface->min_constr[ip];
      max_nconstr[ip] = model_array->modar[index]->interface->max_constr[ip];
      if (min_nconstr[ip] > max_nconstr[ip])
       NLfit_error("Must have noise parameter min cnstrnts <= max cnstrnts");
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the least squares linear regression.
*/

void calc_linear_regression
(
  matrix x,              /* matrix of constants */
  vector y,              /* observation vector */
  vector * b,            /* estimated regression coefficients */
  float * sse            /* error sum of squares */
)

{
  matrix xt, xtx, xtxinv, xtxinvxt;     /* intermediate matrix results */
  vector yhat;                          /* estimate of observation vector */
  vector e;                             /* error in fit */
  int ok;                               /* successful matrix inversion? */


  /*----- initialize matrices -----*/
  matrix_initialize (&xt);
  matrix_initialize (&xtx);
  matrix_initialize (&xtxinv);
  matrix_initialize (&xtxinvxt);
  vector_initialize (&yhat);
  vector_initialize (&e);

  /*----- calculate least squares regression coefficients -----*/
  matrix_transpose (x, &xt);
  matrix_multiply (xt, x, &xtx);
  ok = matrix_inverse (xtx, &xtxinv);
  if (!ok)  NLfit_error ("Unable to invert matrix");
  matrix_multiply (xtxinv, xt, &xtxinvxt);
  vector_multiply (xtxinvxt, y, b);

  /*----- calculate least squares fit -----*/
  vector_multiply (x, *b, &yhat);

  /*----- calculate error sum of squares -----*/
  vector_subtract (y, yhat, &e);
  *sse = vector_dot (e, e);

  /*----- dispose of matrices -----*/
  vector_destroy (&e);
  vector_destroy (&yhat);
  matrix_destroy (&xtxinvxt);
  matrix_destroy (&xtxinv);
  matrix_destroy (&xtx);
  matrix_destroy (&xt);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the least squares fit corresponding the the (linear)
  reduced model.
*/

void calc_reduced_model
(
  int n,                 /* number of observations */
  int r,                 /* number of parameters in reduced model */
  float ** x_array,      /* data matrix */
  float * y_array,       /* observed time series */
  float * b_array,       /* estimated parameters for reduced model */
  float * sse            /* error sum of squares */
)

{
  matrix x;              /* independent variable matrix */
  vector y;              /* observation vector */
  vector b;              /* regression coefficient vector */


  /*----- initialize matrices and vectors -----*/
  matrix_initialize (&x);
  vector_initialize (&y);
  vector_initialize (&b);

  /*----- convert data to matrix and vector types -----*/
  array_to_matrix (n, r, x_array, &x);
  array_to_vector (n, y_array, &y);

  /*----- calculate linear regression -----*/
  calc_linear_regression (x, y, &b, sse);

  /*----- convert vector to array -----*/
  vector_to_array (b, b_array);

  /*----- dispose of matrices and vectors -----*/
  vector_destroy (&b);
  vector_destroy (&y);
  matrix_destroy (&x);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to allocate memory required for calculation of the full model.
*/

void initialize_full_model
(
  int dimension,            /* number of parameters in full model */
  int nbest,                /* keep nbest vectors from random search */
  float *** parameters,     /* array of parameter vectors */
  float ** sse              /* array of error sum of squares */
)

{
  int i;                    /* parameter vector index */

  *sse = (float *) malloc (sizeof(float) * nbest);
  *parameters = (float **) malloc (sizeof(float *) * nbest);
  for (i = 0;  i < nbest;  i++)
    (*parameters)[i] = (float *) malloc (sizeof(float) * dimension);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to determine if the parameter vector violates the constraints.
*/

int calc_constraints
(
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * b_array,        /* estimated parameters for the reduced model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  float * vertex          /* test parameter vector */
)

{
  int i;                  /* parameter index */


  /*----- check noise model constraints -----*/
  if (nabs)   /*--- absolute noise parameter constraints ---*/
    for (i = 0;  i < r;  i++)
      {
       if (vertex[i] < min_nconstr[i])  return (1);
       if (vertex[i] > max_nconstr[i])  return (1);
      }
  else        /*--- relative noise parameter constraints ---*/
    for (i = 0;  i < r;  i++)
      {
       if (vertex[i] < min_nconstr[i] + b_array[i])  return (1);
       if (vertex[i] > max_nconstr[i] + b_array[i])  return (1);
      }

  /*----- check signal model constraints -----*/
  for (i = 0;  i < p;  i++)
    {
      if (vertex[i+r] < min_sconstr[i])  return (1);
      if (vertex[i+r] > max_sconstr[i])  return (1);
    }

  return (0);
}

#define SAVE_RAN  /* 27 July 1998 -- RWCox */
#ifdef  SAVE_RAN
/*===========================================================================*/
/* Data to save and reuse the random search parameters for the signal model. */
/* The purpose is to avoid recomputation of the signal model over and over.  */

/* Parameters of the noise model (not used at this time) */

static vfp     OLD_nmodel      = NULL ;
static int     OLD_r           = -1 ;
static float * OLD_min_nconstr = NULL ;  /* [r] */
static float * OLD_max_nconstr = NULL ;  /* [r] */
static float * RAN_npar = NULL ;         /* [nrand*r]        */
static float * RAN_nts  = NULL ;         /* [nrand*ts_length */

/* Parameters of the signal model */

static vfp     OLD_smodel      = NULL ;
static int     OLD_p           = -1 ;
static float * OLD_min_sconstr = NULL ;  /* [p] */
static float * OLD_max_sconstr = NULL ;  /* [p] */
static float * RAN_spar = NULL ;         /* [nrand*p]        */
static float * RAN_sts  = NULL ;         /* [nrand*ts_length */

static int     OLD_ts_length   = -1 ;
static int     OLD_nrand       = -1 ;
static float   OLD_x0          = -666.0 ;
static float   OLD_x1          = -777.0 ;

static int     RAN_sind = -1 ;  /* if >= 0, index of random signal to use */

/*---------------------------------------------------------------------------*/
/* Function to compare two floating point vectors of the same length         */

int RAN_compare_vect( int n , float * a , float * b )
{
   int ii ;
   if( n < 1 || a == NULL || b == NULL ) return 1 ;
   for( ii=0 ; ii < n ; ii++ ) if( a[ii] != b[ii] ) return 1 ;
   return 0 ;
}

/*---------------------------------------------------------------------------*/
/* Function to initialize the random parameters and their time series.       */
/* At present, the noise model is not used, due to its 'relative' nature.    */

void RAN_setup
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  int nrand               /* number of random vectors to generate */
)
{
   int ip , ipt ;
   float * par ;
   float * ts ;

ENTRY("RAN_setup") ;

   /*** check if the signal model is dead on arrival ***/

   if( smodel == NULL ){
      OLD_smodel = NULL ;
      OLD_p      = -1 ;
      if( OLD_min_sconstr != NULL ){ free(OLD_min_sconstr); OLD_min_sconstr = NULL; }
      if( OLD_max_sconstr != NULL ){ free(OLD_max_sconstr); OLD_max_sconstr = NULL; }
      if( RAN_spar        != NULL ){ free(RAN_spar)       ; RAN_spar        = NULL; }
      if( RAN_sts         != NULL ){ free(RAN_sts )       ; RAN_sts         = NULL; }
      EXRETURN ;
   }

   /*** check if the signal model has changed ***/

   if( smodel        != OLD_smodel                     ||
       p             != OLD_p                          ||
       ts_length     != OLD_ts_length                  ||
       nrand         != OLD_nrand                      ||
       x_array[0][1] != OLD_x0                         ||
       x_array[1][1] != OLD_x1                         ||
       RAN_compare_vect(p,min_sconstr,OLD_min_sconstr) ||
       RAN_compare_vect(p,max_sconstr,OLD_max_sconstr)   ){

      /* save parameters of new signal model */

STATUS("initializing random signal models") ;

      OLD_smodel    = smodel ;
      OLD_p         = p ;
      OLD_ts_length = ts_length ;
      OLD_nrand     = nrand ;
      OLD_x0        = x_array[0][1] ;
      OLD_x1        = x_array[1][1] ;
STATUS("creating space for OLD values") ;
      if( OLD_min_sconstr != NULL ) free(OLD_min_sconstr) ;
      if( OLD_max_sconstr != NULL ) free(OLD_max_sconstr) ;
      OLD_min_sconstr = (float *) malloc(sizeof(float)*p) ;
      OLD_max_sconstr = (float *) malloc(sizeof(float)*p) ;
      memcpy( OLD_min_sconstr , min_sconstr , sizeof(float)*p ) ;
      memcpy( OLD_max_sconstr , max_sconstr , sizeof(float)*p ) ;

      /* create space for new random signal model vectors */

STATUS("creating space for RAN vectors") ;
      if( RAN_spar != NULL ) free(RAN_spar) ;
      if( RAN_sts  != NULL ) free(RAN_sts ) ;
      RAN_spar = (float *) malloc(sizeof(float)*nrand*p) ;
      RAN_sts  = (float *) malloc(sizeof(float)*nrand*ts_length) ;

      /* compute new random signal vectors */

STATUS("computing random signal vectors") ;
      for( ipt=0 ; ipt < nrand ; ipt++ ){

         par = RAN_spar + (ipt*p) ;         /* ipt-th parameter vector */
         ts  = RAN_sts  + (ipt*ts_length) ; /* ipt-th signal model time series */

         for( ip=0 ; ip < p ; ip++ )                 /* parameter vector */
            par[ip] = get_random_value(min_sconstr[ip], max_sconstr[ip]) ;

         if( ipt == 0 ) STATUS("calling signal model") ;
#if 0
         smodel( par , ts_length , x_array , ts ) ;  /* time series vector */
#else
         AFNI_CALL_VOID_4ARG(smodel ,
                             float *,par , int,ts_length,
                             float **,x_array , float *,ts ) ;
#endif
      }

#if 0
      fprintf(stderr," - done\n") ;
#endif
   } /* end of signal model stowage */

   EXRETURN ;
}
/*===========================================================================*/
#endif /* SAVE_RAN */

/*---------------------------------------------------------------------------*/
/*
  Calculate the estimated time series using the full model.
*/

void full_model
(
  vfp nmodel,                 /* pointer to noise model */
  vfp smodel,                 /* pointer to signal model */
  float * gn,                 /* parameters for noise model */
  float * gs,                 /* parameters for signal model */
  int ts_length,              /* length of time series data */
  float ** x_array,           /* independent variable matrix */
  float * yhat_array          /* output estimated time series */
)

{
  int it;                     /* time index */
  float * y_array = NULL;     /* estimated signal time series */
#ifdef SAVE_RAN
  int use_model = ( RAN_sind < 0 || ts_length != OLD_ts_length ) ;
#endif

  /*----- generate time series corresponding to signal model -----*/

#ifdef SAVE_RAN
  if( use_model ){  /* don't use saved time series */
#endif
     y_array = (float *) malloc (sizeof(float) * (ts_length));
     if (y_array == NULL)
       NLfit_error ("Unable to allocate memory for y_array");
#if 0
     smodel (gs, ts_length, x_array, y_array);
#else
     AFNI_CALL_VOID_4ARG(smodel ,
                         float *,gs , int,ts_length,
                         float **,x_array , float *,y_array ) ;
#endif

#ifdef SAVE_RAN
  } else            /* recall a saved time series */
     y_array = RAN_sts + (ts_length*RAN_sind) ;
#endif

  /*----- generate time series corresponding to the noise model -----*/
#if 0
  nmodel (gn, ts_length, x_array, yhat_array);
#else
  AFNI_CALL_VOID_4ARG(nmodel ,
                      float *,gn , int,ts_length,
                      float **,x_array , float *,yhat_array ) ;
#endif

  /*----- add signal and noise model time series -----*/

#ifdef UNROLL
  { int ib = ts_length % 4 ;
    switch( ib ){
      case 3: yhat_array[2] += y_array[2]; /* fall thru */
      case 2: yhat_array[1] += y_array[1]; /* fall thru */
      case 1: yhat_array[0] += y_array[0]; break ;
    }
    for (it=ib;  it < ts_length;  it+=4){
      yhat_array[it]   += y_array[it];
      yhat_array[it+1] += y_array[it+1];
      yhat_array[it+2] += y_array[it+2];
      yhat_array[it+3] += y_array[it+3];
    }
  }
#else   /* don't UNROLL */
  for (it = 0;  it < ts_length;  it++)
    yhat_array[it] += y_array[it];
#endif  /* UNROLL */


  /*----- deallocate memory -----*/
#ifdef SAVE_RAN
  if( use_model )
#endif
    free (y_array) ;

  y_array = NULL;  /* not really needed since this array is 'auto' */
}


/*---------------------------------------------------------------------------*/
/*
  This routine calculates the error sum of squares corresponding to
  the given vector of parameters for the full model.
*/

float calc_sse
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  float * b_array,        /* estimated parameters for the reduced model */
  float * vertex,         /* test parameter vector */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array        /* observed time series */
)

{
  const float BIG_NUMBER = 1.0e+10;   /* return large number if constraints
                                         are violated */
  int i;                              /* time point index */
  float t;                            /* time */
  float diff;                         /* error at individual time point */
  float sse;                          /* error sum of squares */
  float * y_array = NULL;             /* estimated time series */


  /*----- allocate memory for estimated time series -----*/
  y_array = (float *) malloc (sizeof(float) * ts_length);

  /*----- apply constraints? -----*/
  if (calc_constraints (r, p, nabs, b_array, min_nconstr, max_nconstr,
                        min_sconstr, max_sconstr, vertex))
    {
      free (y_array);   y_array = NULL;
      return (BIG_NUMBER);
    }

  /*----- create estimated time series using the full model parameters -----*/
  full_model (nmodel, smodel, vertex, vertex + r, ts_length, x_array, y_array);

  /*----- calculate error sum of squares -----*/
#ifdef UNROLL
  { int ib = ts_length % 4 ; float d2,d3,d4 ;
    sse = 0.0f ;
    switch( ib ){
      case 3: d4 = ts_array[2] - y_array[2] ; sse += d4*d4 ; /* fall thru */
      case 2: d3 = ts_array[1] - y_array[1] ; sse += d3*d3 ; /* fall thru */
      case 1: d2 = ts_array[0] - y_array[0] ; sse += d2*d2 ; break ;
    }
    for(i=ib ; i < ts_length ; i+=4){
      diff = ts_array[i]   - y_array[i]  ;
      d2   = ts_array[i+1] - y_array[i+1];
      d3   = ts_array[i+2] - y_array[i+2];
      d4   = ts_array[i+3] - y_array[i+3];
      sse += diff*diff + d2*d2 + d3*d3 + d4*d4;
    }
  }
#else   /* don't UNROLL */
  sse = 0.0;
  for (i = 0;  i < ts_length;  i++)
    {
      diff = ts_array[i] - y_array[i];
      sse += diff * diff;
    }
#endif  /* UNROLL */

  /*----- release memory -----*/
  free (y_array);   y_array = NULL;

  /*----- return error sum of squares -----*/
  return (sse);
}


/*---------------------------------------------------------------------------*/
/*
  Select and evaluate randomly chosen vectors in the parameter space.
*/

void random_search
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array,       /* observed time series */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  int nrand,              /* number of random vectors to generate */
  int nbest,              /* number of random vectors to keep */
  float ** parameters,    /* array of best random vectors */
  float * response        /* array of best sse's */
)

{
  int ip;                 /* parameter index */
  int iv, ipt;            /* vector indices */
  float sse;              /* error sum of squares */
  float * par = NULL;     /* test parameter vector */


#ifdef SAVE_RAN
  /*** 28 July 1998: set up to choose the best noise model for each case,
                     rather than a completely random one -- RWCox.       ***/
#undef  BEST_NOISE
#ifdef  BEST_NOISE
   float * qts = (float *) malloc(sizeof(float)*ts_length) ;
   float junk ;
#endif
#endif /* SAVE_RAN */

  /*** 27 July 1998: generate some random signal parameters,
                     but only if the signal model is new this time. ***/
#ifdef SAVE_RAN
   RAN_setup( nmodel , smodel , r , p , nabs ,
              min_nconstr, max_nconstr,
              min_sconstr, max_sconstr,
              ts_length, x_array, nrand ) ;
#endif

  /*----- allocate memory for test parameter vector -----*/
  par = (float *) malloc (sizeof(float) * (r+p));

  /*----- initialize response values -----*/
  for (iv = 0;  iv < nbest;  iv++)
    response[iv] = 1.0e+30;


  /*----- loop over random vectors -----*/
  for (ipt = 0;  ipt < nrand;  ipt++)
    {

      /*** 28 July 1998: get the best noise parameter model ***/
#ifdef BEST_NOISE
      for( ip=0 ; ip < ts_length ; ip++ )  /* subtract signal model from data */
         qts[ip] = ts_array[ip] - RAN_sts[ts_length*ipt+ip] ;
      calc_reduced_model( ts_length , r , x_array , qts , par , &junk ) ;
#else
      /*----- select random parameters -----*/
      if (nabs)   /*--- absolute noise parameter constraints ---*/
       for (ip = 0;  ip < r;  ip++)
         par[ip] = get_random_value (min_nconstr[ip], max_nconstr[ip]);
       else        /*--- relative noise parameter constraints ---*/
        for (ip = 0;  ip < r;  ip++)
          par[ip] = get_random_value (min_nconstr[ip] + par_rdcd[ip],
                                      max_nconstr[ip] + par_rdcd[ip]);
#endif /* BEST_NOISE */

      /*** 27 July 1998: get the signal parameters from the saved set ***/
#ifdef SAVE_RAN
      for( ip=0 ; ip < p ; ip++ )
         par[ip+r] = RAN_spar[ipt*p+ip] ;
#else
      for (ip = 0;  ip < p;  ip++)
        par[ip+r] = get_random_value (min_sconstr[ip], max_sconstr[ip]);
#endif


      /*----- evaluate this random vector -----*/
#ifdef SAVE_RAN
      RAN_sind = ipt ;
#endif

      sse = calc_sse (nmodel, smodel, r, p, nabs, min_nconstr, max_nconstr,
                      min_sconstr, max_sconstr, par_rdcd, par,
                      ts_length, x_array, ts_array);

      /*----- save best random vectors -----*/

      /*** The previous code for this is erroneous, since it does not
           allow for the case where an in-between best and worst vector
           is found.  For example, suppose the response array is now
           [ 1 3 5 ] (with nbest==3).  Then we get sse=2.  This would
           replace the 3 value, leaving us with [1 2 5], instead of
           [ 1 2 3 ], which is a better set.  The solution is to push
           the displaced vectors up in the array.  In this way, the
           response array is always sorted, and keeps the truly best. ***/

#define PUSH_BEST  /* 27 July 1998 -- RWCox */
#ifdef  PUSH_BEST
      for (iv = 0;  iv < nbest;  iv++)
       if (sse < response[iv]){
           int jv ;
           for( jv=nbest-1 ; jv > iv ; jv-- ){  /* the push-up code */
              response[jv] = response[jv-1] ;
              for( ip=0 ; ip < r+p ; ip++ )
                 parameters[jv][ip] = parameters[jv-1][ip] ;
           }

           response[iv] = sse ;                 /* now can copy new */
           for( ip=0 ; ip < r+p ;  ip++ )       /* values into place */
              parameters[iv][ip] = par[ip] ;
           break ;
        }
#else
      for (iv = 0;  iv < nbest;  iv++)          /* this is the old code */
       if (sse < response[iv])
        {
         for (ip = 0;  ip < r+p;  ip++)
           parameters[iv][ip] = par[ip];
         response[iv] = sse;
         break;
       }
#endif
  } /* end of loop over random vectors */

  /*----- release memory space -----*/
  free (par);       par = NULL;

#ifdef SAVE_RAN
      RAN_sind = -1 ;
#endif

#if 0
   /*** Some debugging stuff -- 28 July 1998 ***/
   { static count=-1 ;
     count++ ;
     if( count%10 == 0 ){
        printf("Random search sse:\n") ;
        for( iv=0 ; iv < nbest ; iv++ ) printf(" %g",response[iv]) ;
        printf("\n") ;
     }
   }
#endif

}

/*---------------------------------------------------------------------------*/
static int nwin_sim = 0 ;  /* # best with pure simplex */
static int nwin_pow = 0 ;  /* # best with Powell */
static int nwin_stp = 0 ;  /* # best with simplex then Powell */

/*---------------------------------------------------------------------------*/
/*
  Estimate the parameters for the full model.
*/

void calc_full_model
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array,       /* observed time series */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  float sse_rdcd,         /* error sum of squares for the reduced model */
  int nabs,               /* use absolute constraints for noise parameters */
  int nrand,              /* number of random vectors to generate */
  int nbest,              /* number of random vectors to keep */
  float rms_min,          /* minimum rms error required to reject rdcd model */
  float * par_full,       /* estimated parameters for the full model */
  float * sse_full,       /* error sum of squares for the full model */
  int * novar             /* flag for insufficient variation in data */
)

{
  int iv;                        /* vector index */
  int ip;                        /* parameter index */
  float ** parameters = NULL;    /* array of parameter vectors */
  float * sse = NULL;            /* array of sse's */
  int winner , ivb ;

ENTRY("calc_full_model") ;


  /*----- if this is the null signal model,
          or if rms error for reduced model is very small,
          just use the reduced model -----*/
  if ( (p < 1) || (sqrt(sse_rdcd/(ts_length - r)) < rms_min) )
    {
      *novar = 1;
      for (ip = 0;  ip < r  ;  ip++) par_full[ip] = par_rdcd[ip];
      for (ip = r;  ip < r+p;  ip++) par_full[ip] = 0.0;
      *sse_full = sse_rdcd;
      EXRETURN;
    }
  else
    *novar = 0;


  /*----- initialize random number generator -----*/
  srand48 (1234567);

  /*----- allocate memory for calculation of full model-----*/
  initialize_full_model (r+p, nbest, &parameters, &sse);

  /*----- evaluate randomly chosen vectors in the parameter space -----*/

STATUS("call random_search") ;
  random_search (nmodel, smodel, r, p, nabs,
                 min_nconstr, max_nconstr, min_sconstr, max_sconstr,
                 ts_length, x_array, ts_array, par_rdcd, nrand, nbest,
                 parameters, sse);

  /*----- use the best random vectors as the starting points for
    nonlinear optimization -----*/

  winner = 0 ; ivb = 0 ; *sse_full = 1.0e+30 ;
  for (iv = 0;  iv < nbest;  iv++)
    {
STATUS("call generic_optimization") ;
      generic_optimization (nmodel, smodel, r, p,
                            min_nconstr, max_nconstr, min_sconstr, max_sconstr,
                            nabs, ts_length, x_array, ts_array, par_rdcd,
                            parameters[iv], &sse[iv]);
      if( sse[iv] < *sse_full ){
        ivb = iv; winner = opt_winner; *sse_full = sse[iv];
      }
    }

  /*----- save the best result from the nonlinear optimization -----*/

  for( ip=0 ; ip < r+p ; ip++ ) par_full[ip] = parameters[ivb][ip] ;
  switch( winner ){
    case WIN_POW: nwin_pow++ ; break ;
    case WIN_SIM: nwin_sim++ ; break ;
    case WIN_STP: nwin_stp++ ; break ;
  }

  /*----- release memory space -----*/
STATUS("release memory") ;
  for (iv = 0;  iv < nbest;  iv++)
    {
      free (parameters[iv]);
      parameters[iv] = NULL;
    }
  free (parameters);       parameters = NULL;
  free (sse);              sse = NULL;

  EXRETURN ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the partial derivative matrix, evaluated at the
  estimated parameter location.
*/

void calc_partial_derivatives
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series data */
  float ** x_array,       /* independent variable matrix */
  float * par_full,       /* estimated parameters for the full model */
  matrix d                /* matrix of estimated partial derivatives */
)

{
  const float EPSILON = 1.0e-10;
  int dimension;          /* dimension of full model */
  int ip, jp;             /* parameter indices */
  int it;                 /* time index */
  float delp;             /* delta parameter */
  float * par = NULL;            /* perturbed parameter array */
  float * ts1_array = NULL;      /* estimated time series */
  float * ts2_array = NULL;      /* perturbed estimated time series */


  /*----- dimension of full model -----*/
  dimension = r + p;

  /*----- allocate memory -----*/
  ts1_array = (float *) malloc (sizeof(float) * ts_length);
  ts2_array = (float *) malloc (sizeof(float) * ts_length);
  par = (float *) malloc (sizeof(float) * dimension);

  /*----- fitted time series at estimated parameter vector -----*/
  full_model (nmodel, smodel, par_full, par_full + r,
           ts_length, x_array, ts1_array);

  /*----- loop over parameters in model -----*/
  for (jp = 0;  jp < dimension;  jp++)
    {
      /*----- initialize parameters -----*/
      for (ip = 0;  ip < dimension;  ip++) par[ip] = par_full[ip];

      /*----- add small increment to the jpth parameter -----*/
      if (jp < r) delp = (max_nconstr[jp] - min_nconstr[jp]) / 1000.0;
      else        delp = (max_sconstr[jp-r] - min_sconstr[jp-r]) / 1000.0;
      par[jp] += delp;

      /*----- fit time series for perturbed parameter -----*/
      full_model (nmodel, smodel, par, par + r, ts_length, x_array, ts2_array);

      /*----- estimate partial derivative -----*/
      if (delp > EPSILON)
       for (it = 0;  it < ts_length;  it++)
         d.elts[it][jp] = (ts2_array[it] - ts1_array[it]) / delp;
      else
       for (it = 0;  it < ts_length;  it++)
         d.elts[it][jp] = 0.0;

    }

  /*----- free memory -----*/
  free (par);        par = NULL;
  free (ts2_array);  ts2_array = NULL;
  free (ts1_array);  ts1_array = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the coefficient of multiple determination R^2.
*/

float calc_rsqr
(
  float ssef,                 /* error sum of squares from full model */
  float sser                  /* error sum of squares from reduced model */
)

{
  const float EPSILON = 1.0e-5;     /* protection against divide by zero */
  float rsqr;                       /* coeff. of multiple determination R^2  */


  /*----- coefficient of multiple determination R^2 -----*/
  if (sser < EPSILON)
    rsqr = 0.0;
  else
    rsqr = (sser - ssef) / sser;


  /*----- Limit range of values for R^2 -----*/
  if (rsqr < 0.0)   rsqr = 0.0;
  if (rsqr > 1.0)   rsqr = 1.0;


  /*----- Return coefficient of multiple determination R^2 -----*/
  return (rsqr);
}


/*---------------------------------------------------------------------------*/
/*
  Perform additional calculations following the least squares parameter
  estimates.
*/

void analyze_results
(
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int novar,              /* flag for insufficient variation in the data */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int ts_length,          /* length of time series data */
  float ** x_array,       /* independent variable matrix */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  float sse_rdcd,         /* error sum of squares for the reduced model */
  float * par_full,       /* estimated parameters for the full model */
  float sse_full,         /* error sum of squares for the full model */
  float * rmsreg,         /* root-mean-square for the full regression model */
  float * freg,           /* f-statistic for the full regression model */
  float * rsqr,           /* R^2 (coef. of multiple determination) */
  float * smax,           /* signed maximum of signal */
  float * tmax,           /* epoch of signed maximum of signal */
  float * pmax,           /* percentage change due to signal */
  float * area,           /* area between signal and baseline */
  float * parea,          /* percentage area between signal and baseline */
  float * tpar_full       /* t-statistic of the parameters in the full model */
)

{
  const float EPSILON = 1.0e-10;
  int dimension;                 /* dimension of full model */
  int ip;                        /* parameter index */
  int df_rdcd;                   /* degrees of freedom for reduced model */
  int df_full;                   /* degrees of freedom for full model */
  float mse_full;                /* MSE for full model */
  float mse_reg;                 /* MSE for the regression */
  int ok;                        /* boolean for successful matrix inversion */
  float * y_array = NULL;        /* estimated signal time series */
  float * base_array = NULL;     /* baseline time series */
  float barea;                   /* area under baseline */
  int it;                        /* time series index */
  float y1, y2, y3;              /* temporary values for calculating area */


  /*----- initialization -----*/
  dimension = r + p;
  *rmsreg = *freg = *rsqr = *smax = *tmax = *pmax = *area = *parea = 0.0;
  for (ip = 0;  ip < dimension;  ip++)
    tpar_full[ip] = 0.0;


  /*----- check for insufficient variation in the data -----*/
  if (novar)  return;


  /*----- Adjust dof if constraints force a parameter to be a constant -----*/
  df_rdcd = ts_length - r;
  df_full = ts_length - dimension;

  for (ip = 0;  ip < r;  ip++)
    if (min_nconstr[ip] == max_nconstr[ip])
      { df_rdcd++; df_full++; }

  for (ip = 0;  ip < p;  ip++)
    if (min_sconstr[ip] == max_sconstr[ip])
      df_full++;


  /*----- MSE for full model -----*/
  mse_full = sse_full / df_full;

  /*----- MSE due to regression -----*/
  if (df_rdcd == df_full)
    mse_reg = 0.0;
  else
    mse_reg = (sse_rdcd - sse_full) / (df_rdcd - df_full);
  if (mse_reg < 0.0) mse_reg = 0.0;

  /*----- f-statistic for significance of the regression -----*/
  if (mse_full > EPSILON)
    *freg = mse_reg / mse_full;
  else
    *freg = 0.0;

  /*----- root-mean-square error for the regression -----*/
  *rmsreg = sqrt(mse_full);

  /*----- R^2 (coefficient of multiple determination) -----*/
  *rsqr = calc_rsqr (sse_full, sse_rdcd);

  /*----- generate time series corresponding to the signal model -----*/
  y_array = (float *) malloc (sizeof(float) * (ts_length));
  if (y_array == NULL)
    NLfit_error ("Unable to allocate memory for y_array");
#if 0
  smodel (par_full+r, ts_length, x_array, y_array);
#else
  AFNI_CALL_VOID_4ARG(smodel ,
                      float *,(par_full+r) , int,ts_length,
                      float **,x_array , float *,y_array ) ;
#endif

  /*----- generate time series corresponding to the noise model -----*/
  base_array = (float *) malloc (sizeof(float) * (ts_length));
  if (base_array == NULL)
    NLfit_error ("Unable to allocate memory for base_array");
#if 0
  nmodel (par_full, ts_length, x_array, base_array);
#else
  AFNI_CALL_VOID_4ARG(nmodel ,
                      float *,par_full , int,ts_length,
                      float **,x_array , float *,base_array ) ;
#endif

  /*----- initialize signal parameters -----*/
  *tmax = x_array[0][1];
  *smax = y_array[0];
  if (fabs(base_array[0]) > EPSILON)
    *pmax = 100.0 * y_array[0] / fabs(base_array[0]);
  else
    *pmax = 0.0;
  *area = 0.0;
  barea = 0.0;
  *parea = 0.0;

  /*----- calculate signed maximum of the signal, percent change, area -----*/
  for (it = 1;  it < ts_length;  it++)
    {
      /*----- calculate signed maximum of signal, and
         calculate max. percentage change of signal wrt baseline -----*/
      if (fabs(y_array[it]) > fabs(*smax))
      {
       *tmax = x_array[it][1];
       *smax = y_array[it];
       if (fabs(base_array[it]) > EPSILON)
         *pmax = 100.0 * y_array[it] / fabs(base_array[it]);
      }

      /*----- calculate area and percentage area under the signal -----*/
      y1 = y_array[it-1];   y2 = y_array[it];
      if ((y1 > 0.0) && (y2 > 0.0))
      {
       *area += (y1 + y2) / 2.0;
       *parea += (y1 + y2) / 2.0;
      }
      else if ((y1 < 0.0) && (y2 < 0.0))
      {
       *area -= (y1 + y2) / 2.0;
       *parea += (y1 + y2) / 2.0;
      }
      else
      {
       y3 = fabs(y1) + fabs(y2);
       if (y3 > EPSILON)
       {
           *area += (y1*y1 + y2*y2) / (2.0 * y3);
           if (y1 > y2) *parea += (y1*y1 - y2*y2) / (2.0 * y3);
           else         *parea -= (y1*y1 - y2*y2) / (2.0 * y3);
       }
     }

      y1 = base_array[it-1];   y2 = base_array[it];
      if ((y1 > 0.0) && (y2 > 0.0))
      { barea += (y1 + y2) / 2.0; }
      else if ((y1 < 0.0) && (y2 < 0.0))
      { barea -= (y1 + y2) / 2.0; }
      else
      { y3 = fabs(y1) + fabs(y2);
        if (y3 > EPSILON)
        { barea += (y1*y1 + y2*y2) / (2.0 * y3); }
      }
   }  /* it */

  if (barea > EPSILON) *parea *= 100.0/barea;
  else                 *parea  = 0.0;

  free (base_array);   base_array = NULL;
  free (y_array);      y_array = NULL;


  /*----- Calculate t-statistics? -----*/
  if (calc_tstats)
    {
      float stddev;                 /* est. std. dev. for a parameter */
      matrix d, dt, dtd, dtdinv;    /* matrices used for calc. of covariance */

      /*----- initialize matrices -----*/
      matrix_initialize (&d);
      matrix_initialize (&dt);
      matrix_initialize (&dtd);
      matrix_initialize (&dtdinv);

      /*----- calculate the matrix of partial derivatives D -----*/
      matrix_create (ts_length, dimension, &d);
      calc_partial_derivatives (nmodel, smodel, r, p,
                               min_nconstr, max_nconstr,
                               min_sconstr, max_sconstr,
                               ts_length, x_array, par_full, d);

      /*----- calculate variance-covariance matrix -----*/
      matrix_transpose (d, &dt);
      matrix_multiply (dt, d, &dtd);
      ok = matrix_inverse (dtd, &dtdinv);
      if (ok)
       for (ip = 0;  ip < dimension;  ip++)
       {
         stddev = sqrt((sse_full/(df_full)) * dtdinv.elts[ip][ip]);
         if (stddev > EPSILON)
           tpar_full[ip] = par_full[ip] / stddev;
         else
           tpar_full[ip] = 0.0;
       }
      else
       for (ip = 0;  ip < dimension;  ip++)
       {
         tpar_full[ip] = 0.0;
       }


      /*----- dispose of matrices -----*/
      matrix_destroy (&dtdinv);
      matrix_destroy (&dtd);
      matrix_destroy (&dt);
      matrix_destroy (&d);

    }  /* if (calc_tstats) */

}


/*---------------------------------------------------------------------------*/
/*
  Convert F-value to p-value.
  This routine was copied from: mri_stats.c
*/

#if 0
double fstat_t2p( double ff , double dofnum , double dofden )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   f      = ff ;
   dfn    = dofnum ;
   dfd    = dofden ;

   cdff( &which , &p , &q , &f , &dfn , &dfd , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}
#endif

/*---------------------------------------------------------------------------*/
/*
  Report results for this voxel.
*/

void report_results
(
  char * nname,            /* name of noise model */
  char * sname,            /* name of signal model */
  int r,                   /* number of parameters in the noise model */
  int p,                   /* number of parameters in the signal model */
  char ** npname,          /* noise parameter names */
  char ** spname,          /* signal parameter names */
  int ts_length,           /* length of time series array */
  float * par_rdcd,        /* estimated parameters for the reduced model */
  float sse_rdcd,          /* error sum of squares for the reduced model */
  float * par_full,        /* estimated parameters for the full model */
  float sse_full,          /* error sum of squares for the full model */
  float * tpar_full,       /* t-statistic of the parameters in full model */
  float rmsreg,            /* root-mean-square error for the full model */
  float freg,              /* f-statistic for the full regression model */
  float rsqr,              /* R^2 (coef. of multiple determination) */
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */
  float pmax,              /* percentage change due to signal */
  float area,              /* area between signal and baseline */
  float parea,             /* percentage area between signal and baseline */
  char ** label            /* label output for this voxel */
)

{
  int ip;                  /* parameter index */
  double pvalue;


  /*----- create label if desired by calling program -----*/
  if (label == NULL)  return;

  /*----- make this a 0 length string to start -----*/
  lbuf[0] = '\0';

  /*----- write reduced model parameters -----*/
  sprintf (sbuf, "Reduced (%s) Model: \n", nname);
  strcat (lbuf, sbuf);
  for (ip = 0;  ip < r;  ip++)
    {
      sprintf (sbuf, "b[%d]= %12.6f  %s \n", ip, par_rdcd[ip], npname[ip]);
      strcat (lbuf, sbuf);
    }

  /*----- write full model parameters -----*/
  sprintf (sbuf, "\nFull (%s + %s) Model: \n", nname, sname);
  strcat (lbuf, sbuf);
  for (ip = 0;  ip < r;  ip++)
    {
      sprintf (sbuf, "gn[%d]=%12.6f  %s \n", ip, par_full[ip], npname[ip]);
      strcat (lbuf, sbuf);

    }
  for (ip = 0;  ip < p;  ip++)
    {
      sprintf (sbuf, "gs[%d]=%12.6f  %s \n", ip, par_full[ip+r], spname[ip]);
      strcat (lbuf, sbuf);
    }

  sprintf (sbuf, "\nSignal Tmax  = %12.3f \n", tmax);
  strcat (lbuf, sbuf);
  sprintf (sbuf,   "Signal Smax  = %12.3f \n", smax);
  strcat (lbuf, sbuf);
  sprintf (sbuf,   "Signal PSmax = %12.3f \n", pmax);
  strcat (lbuf, sbuf);
  sprintf (sbuf,   "Signal Area  = %12.3f \n", area);
  strcat (lbuf, sbuf);
  sprintf (sbuf,   "Signal PArea = %12.3f \n", parea);
  strcat (lbuf, sbuf);

  sprintf (sbuf, "\nRMSE Rdcd = %8.3f \n", sqrt(sse_rdcd/(ts_length-r)));
  strcat (lbuf, sbuf);
  sprintf (sbuf, "RMSE Full = %8.3f \n", sqrt(sse_full/(ts_length-r-p)));
  strcat (lbuf, sbuf);

  sprintf (sbuf, "\nR^2       = %7.3f \n", rsqr);
  strcat (lbuf, sbuf);
  sprintf (sbuf, "F[%2d,%3d] = %7.3f \n", p, ts_length-r-p, freg);
  strcat (lbuf, sbuf);
  pvalue = fstat_t2p ( (double) freg, (double) p, (double) ts_length-r-p);
  sprintf (sbuf, "p-value   = %e  \n", pvalue);
  strcat (lbuf, sbuf);


  /*----- send address of lbuf back in what label points to -----*/
  *label = lbuf;

}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
#ifdef LINEAR_REDUCTION

static int    LR_nrow=0 , LR_ncol=0 ;
static float *LR_mat  = NULL ;
static float *LR_pinv = NULL ;

#undef  LRx
#undef  LRi
#define LRx(i,j) LR_mat [(j)+(i)*LR_ncol]  /* i=0..LR_nrow-1 , j=0..LR_ncol-1 */
#define LRi(i,j) LR_pinv[(j)+(i)*LR_nrow]  /* i=0..LR_ncol-1 , j=0..LR_nrow-1 */

#undef  LRx_row
#undef  LRi_row
#define LRx_row(i) (LR_mat+(i)*LR_ncol)
#define LRi_row(i) (LR_pinv+(i)*LR_nrow)

void LR_clear_matrix(void)
{
  if( LR_mat != NULL ){ free(LR_mat ); LR_mat =NULL; }
  if( LR_pinv!= NULL ){ free(LR_pinv); LR_pinv=NULL; }
}

void LR_setup_matrix( int ts_length , int r , float **x_array )
{
   int ii , jj ;
   matrix XtXinvXt , X ;
   double **Xpar , **Xar ;

   if( ts_length < 1 || r < 1 || x_array == NULL ) return ;
   matrix_initialize( &X ) ;
   matrix_initialize( &XtXinvXt ) ;

   array_to_matrix( ts_length, r, x_array, &X ) ;
   matrix_psinv( X , NULL , &XtXinvXt ) ;
   Xar = X.elts ; Xpar = XtXinvXt.elts ;

   LR_clear_matrix() ;
   LR_nrow = ts_length ; LR_ncol = r ;

   LR_mat  = (float *)malloc(sizeof(float)*LR_nrow*LR_ncol) ;
   LR_pinv = (float *)malloc(sizeof(float)*LR_nrow*LR_ncol) ;

   for( ii=0 ; ii < LR_nrow ; ii++ )
     for( jj=0 ; jj < LR_ncol ; jj++ ) LRx(ii,jj) = Xar[ii][jj] ;

   for( ii=0 ; ii < LR_ncol ; ii++ )
     for( jj=0 ; jj < LR_nrow ; jj++ ) LRi(ii,jj) = Xpar[ii][jj] ;

   matrix_destroy(&X) ; matrix_destroy(&XtXinvXt) ;
}

/*---------------------------------------------------------------------------*/
/*
  Calculate the estimated time series using the full model.
*/

void LR_full_model
(
  vfp smodel,                 /* pointer to signal model */
  float * gs,                 /* parameters for signal model */
  int ts_length,              /* length of time series data */
  int r ,                     /* col dimension of x_array */
  float ** x_array,           /* independent variable matrix */
  float * ts_array ,          /* data time series */
  float * yhat_array          /* output estimated time series */
)

{
  int it, jj , ib=ts_length%4;
  float * y_array = NULL;
  float * d_array , *beta , sum , *rowp ;
#ifdef SAVE_RAN
  int use_model = ( RAN_sind < 0 || ts_length != OLD_ts_length ) ;
#endif

  /*----- generate time series corresponding to signal model -----*/

#ifdef SAVE_RAN
  if( use_model ){  /* don't use saved time series */
#endif
     y_array = (float *) malloc (sizeof(float) * (ts_length));
     if (y_array == NULL)
       NLfit_error ("Unable to allocate memory for y_array");
#if 0
     smodel (gs, ts_length, x_array, y_array);
#else
     AFNI_CALL_VOID_4ARG(smodel ,
                         float *,gs , int,ts_length,
                         float **,x_array , float *,y_array ) ;
#endif

#ifdef SAVE_RAN
  } else            /* recall a saved time series */
     y_array = RAN_sts + (ts_length*RAN_sind) ;
#endif

  /*--- generate time series for noise model from signal model time series ---*/

  if( LR_mat == NULL ) LR_setup_matrix( ts_length , r , x_array ) ;
  d_array = (float *) malloc (sizeof(float) * (ts_length));
  switch( ib ){
    case 3: d_array[2] = ts_array[2] - y_array[2] ; /* fall thru */
    case 2: d_array[1] = ts_array[1] - y_array[1] ; /* fall thru */
    case 1: d_array[0] = ts_array[0] - y_array[0] ; /* fall thru */
  }
  for (it=ib;  it < ts_length;  it+=4){
    d_array[it  ] = ts_array[it  ] - y_array[it  ] ;
    d_array[it+1] = ts_array[it+1] - y_array[it+1] ;
    d_array[it+2] = ts_array[it+2] - y_array[it+2] ;
    d_array[it+3] = ts_array[it+3] - y_array[it+3] ;
  }
  beta = (float *)malloc(sizeof(float)*r) ;
  for( jj=0 ; jj < r ; jj++ ){
    sum = 0.0f ; rowp = LRp_row(jj) ;
    switch( ib ){
      case 3: sum += rowp[2] * d_array[2] ; /* fall thru */
      case 2: sum += rowp[1] * d_array[1] ; /* fall thru */
      case 1: sum += rowp[0] * d_array[0] ; break ;
    }
    for( it=ib ; it < ts_length ; it+=4 )
      sum += rowp[it  ]*d_array[it  ] +
             rowp[it+1]*d_array[it+1] +
             rowp[it+2]*d_array[it+2] +
             rowp[it+3]*d_array[it+3]  ;
    beta[jj] = sum ;
  }

  /*----- generate time series corresponding to the noise model -----*/
#if 0
  nmodel (gn, ts_length, x_array, yhat_array);
#else
  AFNI_CALL_VOID_4ARG(nmodel ,
                      float *,gn , int,ts_length,
                      float **,x_array , float *,yhat_array ) ;
#endif

  /*----- add signal and noise model time series -----*/

#ifdef UNROLL
  { int ib = ts_length % 4 ;
    switch( ib ){
      case 3: yhat_array[2] += y_array[2]; /* fall thru */
      case 2: yhat_array[1] += y_array[1]; /* fall thru */
      case 1: yhat_array[0] += y_array[0]; break ;
    }
    for (it=ib;  it < ts_length;  it+=4){
      yhat_array[it]   += y_array[it];
      yhat_array[it+1] += y_array[it+1];
      yhat_array[it+2] += y_array[it+2];
      yhat_array[it+3] += y_array[it+3];
    }
  }
#else   /* don't UNROLL */
  for (it = 0;  it < ts_length;  it++)
    yhat_array[it] += y_array[it];
#endif  /* UNROLL */


  /*----- deallocate memory -----*/
#ifdef SAVE_RAN
  if( use_model )
#endif
    free (y_array) ;

  y_array = NULL;  /* not really needed since this array is 'auto' */
}
#endif  /* LINEAR_REDUCTION */

/*---------- 3 Nov 2006 drg: save/get dataset index for function calls -------*/

static int dset_ijk=-1 , dset_tin=-1 ;

void AFNI_store_dset_index( int ijk , int tin )
{
   dset_ijk = ijk ; dset_tin = tin ; return ;
}

int AFNI_needs_dset_ijk(void){ return dset_ijk ; }
int AFNI_needs_dset_tin(void){ return dset_tin ; }

