/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This is the header file for NLfit.c.

  File:     NLfit.h
  Author:   B. Douglas Ward
  Date:     3 June 1997

  Mod:      Added options for percent signal change above baseline, and
            calculation of area under the signal above baseline.
  Date:     26 November 1997

  Mod:      Added novar flag to eliminate unnecessary calculations.
  Date:     13 July 1999

  Mod:      Changes for output of R^2 (coefficient of multiple determination),
            and standard deviation of residuals from full model fit.
	    Added global variable calc_tstats.
	    Also, added screen display of p-values.
  Date:     10 May 2000

*/

/*---------------------------------------------------------------------------*/

#ifndef _NLFIT_HEADER_
#define _NLFIT_HEADER_

#include "NLfit_model.h"


/*---------------- global data -------------------*/
static char lbuf[4096] ;  
static char sbuf[256] ;

static int calc_tstats = 0;   /* set to 1 for calculating t-stats */


/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

void NLfit_error 
(
  char * message         /* message to be displayed */
);


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
);


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the noise0 model by defining the number of parameters
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
);


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
);


/*---------------------------------------------------------------------------*/

void calc_reduced_model 
( 
  int n,                 /* number of observations */        
  int r,                 /* number of parameters in reduced model */
  float ** x_array,      /* data matrix */
  float * y_array,       /* observed time series */
  float * b_array,       /* estimated parameters for reduced model */
  float * sse            /* error sum of squares */
);


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
);


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
);


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
);


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
);


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
);


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
);


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
);


/*---------------------------------------------------------------------------*/

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
);


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
  float rmsreg,            /* root-mean-square for the full regression model */
  float freg,              /* f-statistic for the full regression model */
  float rsqr,              /* R^2 (coef. of multiple determination) */
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */
  float pmax,              /* percentage change due to signal */
  float area,              /* area between signal and baseline */
  float parea,             /* percentage area between signal and baseline */
  char ** label            /* label output for this voxel */
);


/*---------------------------------------------------------------------------*/


#endif /* _NLFIT__HEADER_ */

