/*---------------------------------------------------------------------------*/
/*
  This file contains routines for performing multiple linear regression.

  File:    RegAna.c
  Author:  B. Douglas Ward
  Date:    02 September 1998

  Mod:     Restructured matrix calculations to improve execution speed.
  Date:    16 December 1998

*/

/*---------------------------------------------------------------------------*/

#include "matrix.c"

void RA_error (char * message);


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( RA_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Calculate constant matrices to be used for all voxels.
*/

int calc_matrices 
(
  matrix xdata,                 /* experimental design matrix      */
  int p,                        /* number of parameters            */
  int * plist,                  /* list of parameters              */
  matrix * x,                   /* extracted X matrix              */
  matrix * xtxinv,              /* matrix:  1/(X'X)                */
  matrix * xtxinvxt             /* matrix:  (1/(X'X))X'            */
)

{
  matrix xt, xtx;               /* temporary matrix calculation results */
  int ok;                       /* flag for successful matrix inversion */


  /*----- initialize matrices -----*/
  matrix_initialize (&xt);
  matrix_initialize (&xtx);


  /*----- extract the independent variable matrix X -----*/
  matrix_extract (xdata, p, plist, x);


  /*----- calculate various matrices which will be needed later -----*/
  matrix_transpose (*x, &xt);
  matrix_multiply (xt, *x, &xtx);
  ok = matrix_inverse (xtx, xtxinv);

  if (ok)
    matrix_multiply (*xtxinv, xt, xtxinvxt);
  else
    RA_error ("Improper X matrix  (cannot invert X'X) ");


  /*----- dispose of matrices -----*/
  matrix_destroy (&xtx);
  matrix_destroy (&xt);


  return (ok);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the error sum of squares.
*/

float  calc_sse 
(
  matrix x,                  /* independent variable matrix  */
  vector b,                  /* vector of estimated regression parameters */
  vector y                   /* vector of measured data */
)

{
  vector yhat;               /* product Xb */
  vector e;                  /* vector of residuals */
  float sse;                 /* error sum of squares */


  /*----- initialize vectors -----*/
  vector_initialize (&yhat);
  vector_initialize (&e);


  /*----- calculate the error sum of squares -----*/
  vector_multiply (x, b, &yhat);
  vector_subtract (y, yhat, &e);
  sse = vector_dot (e, e);


  /*----- dispose of vectors -----*/
  vector_destroy (&e);
  vector_destroy (&yhat);


  /*----- return SSE -----*/
  return (sse);
 
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the pure error sum of squares.
*/

float  calc_sspe 
(
  vector y,                  /* vector of measured data */
  int * levels,              /* indices for repeat observations */
  int * counts,              /* number of observations at each level */
  int c                      /* number of unique rows in ind. var. matrix */
)

{
  int i, j;                  /* indices */
  float * sum = NULL;        /* sum of observations at each level */
  float diff;                /* difference between observation and average */
  float sspe;                /* pure error sum of squares */


  /*----- initialize sum -----*/
  sum = (float *) malloc (sizeof(float) * c);
  MTEST (sum);
  
  for (j = 0;  j < c;  j++)
    sum[j] = 0.0;


  /*----- accumulate sum for each level -----*/
  for (i = 0;  i < y.dim;  i++)
    {
      j = levels[i];
      sum[j] += y.elts[i];
    }


  /*----- calculate SSPE -----*/
  sspe = 0.0;
  for (i = 0;  i < y.dim;  i++)
    {
      j = levels[i];
      diff = y.elts[i] - (sum[j]/counts[j]);
      sspe += diff * diff;
    }

  
  free (sum);   sum = NULL;


  /*----- return SSPE -----*/
  return (sspe);
 
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the F-statistic for lack of fit.
*/

float calc_flof
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int c,                      /* number of unique rows in ind. var. matrix */
  float sse,                  /* error sum of squares from full model */
  float sspe                  /* error sum of squares due to pure error */
)

{
  const float EPSILON = 1.0e-5;      /* protection against divide by zero */
  float mspe;                 /* mean square error due to pure error */
  float sslf;                 /* error sum of squares due to lack of fit */
  float mslf;                 /* mean square error due to lack of fit */
  float flof;                 /* F-statistic for lack of fit */


  /*----- calculate mean sum of squares due to pure error -----*/
  mspe = sspe / (n - c);


  /*----- calculate mean sum of squares due to lack of fit -----*/
  sslf = sse - sspe;
  mslf = sslf / (c - p);


  /*----- calculate F-statistic for lack of fit -----*/
  if (mspe < EPSILON)
    flof = 0.0;
  else
    flof = mslf / mspe;


  /*----- return F-statistic for lack of fit -----*/
  return (flof);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the regression coefficients.
*/

void calc_coef 
(
  matrix xtxinvxt,            /* matrix:  (1/(X'X))X'   */
  vector y,                   /* vector of measured data   */
  vector * coef               /* vector of regression parameters */
)

{

  /*----- calculate regression coefficients -----*/
  vector_multiply (xtxinvxt, y, coef);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate standard deviations and t-statistics for the regression 
  coefficients.
*/

void calc_tcoef 
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  float sse,                  /* error sum of squares */
  matrix xtxinv,              /* matrix:  1/(Xf'Xf)        */
  vector coef,                /* vector of regression parameters */
  vector * scoef,             /* std. devs. for regression parameters */
  vector * tcoef              /* t-statistics for regression parameters */
)

{
  const float MAXT = 1000.0;         /* maximum value for t-statistic */
  const float EPSILON = 1.0e-5;      /* protection against divide by zero */
  int df;                     /* error degrees of freedom */
  float mse;                  /* mean square error */
  int i;                      /* parameter index */
  float stddev;               /* standard deviation for parameter estimate */
  float tstat;                /* t-statistic for parameter estimate */
  float num;                  /* numerator of t-statistic */
  float var;                  /* variance for parameter estimate */


  /*----- Create vectors -----*/
  vector_create (p, scoef);
  vector_create (p, tcoef);


  /*----- Calculate mean square error -----*/
  df = n - p;
  mse = sse / df;


  for (i = 0;  i < p;  i++)
    {
      /*----- Calculate standard deviation for regression parameters -----*/
      var = mse * xtxinv.elts[i][i];
      if (var <= 0.0)
	stddev = 0.0;
      else
	stddev = sqrt (var);
      scoef->elts[i] = stddev;


      /*----- Calculate t-statistic for regression parameters -----*/
      num = coef.elts[i];
      if (num > MAXT*stddev)
	tstat = MAXT;
      else
	if (num < -MAXT*stddev)
	  tstat = -MAXT;
	else
	  if (stddev < EPSILON)
	    tstat = 0.0;
	  else
	    tstat = num / stddev;


      /*----- Limit range of values for t-statistic -----*/
      if (tstat < -MAXT)  tstat = -MAXT;
      if (tstat > MAXT)   tstat = MAXT;
      
      tcoef->elts[i] = tstat;
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the F-statistic for significance of the regression.
*/

float calc_freg
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int q,                      /* number of parameters in the rdcd model */
  float ssef,                 /* error sum of squares from full model */
  float sser                  /* error sum of squares from reduced model */
)

{
  const float MAXF = 1000.0;         /* maximum value for F-statistic */
  const float EPSILON = 1.0e-5;      /* protection against divide by zero */
  float msef;                 /* mean square error for the full model */
  float msreg;                /* mean square due to the regression */
  float freg;                 /* F-statistic for the full regression model */


  /*----- Order of reduced model = order of full model ??? -----*/
  if (p <= q)   return (0.0);


  /*----- Calculate numerator and denominator of F-statistic -----*/
  msreg = (sser - ssef) / (p - q);    if (msreg < 0.0)  msreg = 0.0;
  msef   = ssef / (n - p);            if (msef  < 0.0)  msef  = 0.0;

  if (msreg > MAXF*msef)  freg = MAXF;
  else 
    if (msef < EPSILON)
      freg = 0.0;
    else
      freg = msreg / msef;


  /*----- Limit range of values for F-statistic -----*/
  if (freg < 0.0)   freg = 0.0;
  if (freg > MAXF)  freg = MAXF;


  /*----- Return F-statistic for significance of the regression -----*/
  return (freg);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the coefficient of multiple determination R^2.
*/

float calc_rsqr 
(
  float ssef,                 /* error sum of squares from full model */
  float ssto                  /* total (corrected for mean) sum of squares */
)

{
  const float EPSILON = 1.0e-5;     /* protection against divide by zero */
  float rsqr;                       /* coeff. of multiple determination R^2  */


  /*----- coefficient of multiple determination R^2 -----*/
  if (ssto < EPSILON)
    rsqr = 0.0;
  else
    rsqr = 1.0 - ssef/ssto;


  /*----- Limit range of values for R^2 -----*/
  if (rsqr < 0.0)   rsqr = 0.0;
  if (rsqr > 1.0)   rsqr = 1.0;


  /*----- Return coefficient of multiple determination R^2 -----*/
  return (rsqr);
}


/*---------------------------------------------------------------------------*/





