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
            29 August 1997

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/


/*---------------------------------------------------------------------------*/

#include "NLfit_model.c"

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

void NLfit_error
(
  char * message         /* message to be displayed */
)

{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
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
  matrix_destroy (&xt);
  matrix_destroy (&xtx);
  matrix_destroy (&xtxinv);
  matrix_destroy (&xtxinvxt);
  vector_destroy (&e);
  
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
  matrix_destroy (&x);
  vector_destroy (&y);
  vector_destroy (&b);
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


  /*----- allocate memory for signal time series -----*/
  y_array = (float *) malloc (sizeof(float) * (ts_length));
  if (y_array == NULL)
    NLfit_error ("Unable to allocate memory for y_array");


  /*----- generate time series corresponding to signal model -----*/
  smodel (gs, ts_length, x_array, y_array);

  /*----- generate time series corresponding to the noise model -----*/
  nmodel (gn, ts_length, x_array, yhat_array);

  /*----- add signal and noise model time series -----*/
  for (it = 0;  it < ts_length;  it++)
    yhat_array[it] += y_array[it];
  

  /*----- deallocate memory -----*/
  free (y_array);   y_array = NULL;
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
  sse = 0.0;
  for (i = 0;  i < ts_length;  i++)
    {
      diff = ts_array[i] - y_array[i];
      sse += diff * diff;
    }

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


  /*----- allocate memory for test parameter vector -----*/
  par = (float *) malloc (sizeof(float) * (r+p));

  /*----- initialize response values -----*/
  for (iv = 0;  iv < nbest;  iv++)
    response[iv] = 1.0e+30;


  /*----- loop over random vectors -----*/
  for (ipt = 0;  ipt < nrand;  ipt++)
    {

      /*----- select random parameters -----*/
      if (nabs)   /*--- absolute noise parameter constraints ---*/
	for (ip = 0;  ip < r;  ip++)
	  par[ip] = get_random_value (min_nconstr[ip], max_nconstr[ip]);
      else        /*--- relative noise parameter constraints ---*/
	for (ip = 0;  ip < r;  ip++)
	  par[ip] = get_random_value (min_nconstr[ip] + par_rdcd[ip], 
				      max_nconstr[ip] + par_rdcd[ip]);
      for (ip = 0;  ip < p;  ip++)
	  par[ip+r] = get_random_value (min_sconstr[ip], max_sconstr[ip]);


      /*----- evaluate this random vector -----*/
      sse = calc_sse (nmodel, smodel, r, p, nabs, min_nconstr, max_nconstr, 
		      min_sconstr, max_sconstr, par_rdcd, par, 
		      ts_length, x_array, ts_array);


      /*----- save best random vectors -----*/
      for (iv = 0;  iv < nbest;  iv++)
	if (sse < response[iv])
	  {
	    for (ip = 0;  ip < r+p;  ip++)
	      parameters[iv][ip] = par[ip];
	    response[iv] = sse;
	    break;
	  }
    }

  /*----- release memory space -----*/
  free (par);       par = NULL;
 
}


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
  float * sse_full        /* error sum of squares for the full model */
)

{
  int iv;                        /* vector index */
  int ip;                        /* parameter index */ 
  float ** parameters = NULL;    /* array of parameter vectors */
  float * sse = NULL;            /* array of sse's */


  /*----- if this is the null signal model, 
          or if rms error for reduced model is very small, 
	  just use the reduced model -----*/
  if ( (p < 1) || (sqrt(sse_rdcd/(ts_length - r)) < rms_min) ) 
    {
      for (ip = 0;  ip < r;  ip++)
	par_full[ip] = par_rdcd[ip];
      for (ip = r;  ip < r+p;  ip++)
	par_full[ip] = 0.0;
      *sse_full = sse_rdcd;
      return;
    }

  /*----- initialize random number generator -----*/
  srand48 (1234567);

  /*----- allocate memory for calculation of full model-----*/
  initialize_full_model (r+p, nbest, &parameters, &sse); 

  /*----- evaluate randomly chosen vectors in the parameter space -----*/
  random_search (nmodel, smodel, r, p, nabs,
		 min_nconstr, max_nconstr, min_sconstr, max_sconstr,
		 ts_length, x_array, ts_array, par_rdcd, nrand, nbest, 
		 parameters, sse);
  
  /*----- use the best random vectors as the starting points for 
    nonlinear optimization -----*/
  for (iv = 0;  iv < nbest;  iv++)
    {
      simplex_optimization (nmodel, smodel, r, p, 
			    min_nconstr, max_nconstr, min_sconstr, max_sconstr,
			    nabs, ts_length, x_array, ts_array, par_rdcd,
			    parameters[iv], &sse[iv]);
    }

  /*----- save the best result from the nonlinear optimization -----*/      
  *sse_full = 1.0e+30;
  for (iv = 0;  iv < nbest;  iv++)
    {
      if (sse[iv] < *sse_full)
	{
	  *sse_full = sse[iv];
	  for (ip = 0;  ip < r+p;  ip++)
	    par_full[ip] = parameters[iv][ip];
	}
    }


  /*----- release memory space -----*/
  for (iv = 0;  iv < nbest;  iv++)
    {
      free (parameters[iv]);
      parameters[iv] = NULL;
    }
  free (parameters);       parameters = NULL;     
  free (sse);              sse = NULL;

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
      for (ip = 0;  ip < dimension;  ip++)
	par[ip] = par_full[ip];

      /*----- add small increment to the jpth parameter -----*/
      if (jp < r)
	delp = (max_nconstr[jp] - min_nconstr[jp]) / 1000.0;
      else
	delp = (max_sconstr[jp-r] - min_sconstr[jp-r]) / 1000.0;
      par[jp] += delp;

      /*----- fit time series for perturbed parameter -----*/
      full_model (nmodel, smodel, par, par + r, 
		  ts_length, x_array, ts2_array);

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
  free (ts1_array);  ts1_array = NULL;
  free (ts2_array);  ts2_array = NULL;

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
  float * smax,           /* signed maximum of signal */
  float * tmax,           /* epoch of signed maximum of signal */
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
  matrix d, dt, dtd, dtdinv;     /* matrices used for calc. of covariance */
  int ok;                        /* boolean for successful matrix inversion */
  float stddev;                  /* est. std. dev. for a parameter */
  float * y_array;               /* estimated signal time series */
  int it;                        /* time series index */


  /*----- initialization -----*/
  dimension = r + p;
  df_rdcd = ts_length - r;
  df_full = ts_length - dimension;

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


  /*----- calculate signed maximum of the signal -----*/
  y_array = (float *) malloc (sizeof(float) * (ts_length));
  if (y_array == NULL)
    NLfit_error ("Unable to allocate memory for y_array");
  smodel (par_full+r, ts_length, x_array, y_array);
  *smax = y_array[0];
  *tmax = x_array[0][1];
  for (it = 0;  it < ts_length;  it++)
    if (fabs(y_array[it]) > fabs(*smax))
      {
	*smax = y_array[it];
	*tmax = x_array[it][1];
      }
  free (y_array);   y_array = NULL;


  /*----- initialize matrices -----*/
  matrix_initialize (&d);
  matrix_initialize (&dt);
  matrix_initialize (&dtd);
  matrix_initialize (&dtdinv);

  /*----- calculate the matrix of partial derivatives D -----*/
  matrix_create (ts_length, dimension, &d);
  calc_partial_derivatives (nmodel, smodel, r, p, min_nconstr, max_nconstr, 
			    min_sconstr, max_sconstr, 
			    ts_length, x_array, par_full, d);

  /*----- calculate variance-covariance matrix -----*/
  matrix_transpose (d, &dt);
  matrix_multiply (dt, d, &dtd);
  ok = matrix_inverse (dtd, &dtdinv);
  if (ok)
    for (ip = 0;  ip < dimension;  ip++)
      {
	stddev = sqrt((sse_full/(ts_length-dimension)) * dtdinv.elts[ip][ip]);
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
  matrix_destroy (&d);    
  matrix_destroy (&dt); 
  matrix_destroy (&dtd);
  matrix_destroy (&dtdinv);
}


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
  float smax,              /* signed maximum of signal */
  float tmax,              /* epoch of signed maximum of signal */
  char ** label            /* label output for this voxel */
)

{
  int ip;                  /* parameter index */


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
  
  sprintf (sbuf, "\nSignal Smax = %12.6f \n", smax);
  strcat (lbuf, sbuf);
  sprintf (sbuf, "Signal Tmax = %12.6f \n", tmax);
  strcat (lbuf, sbuf);

  sprintf (sbuf, "\nRMSE Rdcd = %8.3f \n", sqrt(sse_rdcd/(ts_length-r)));
  strcat (lbuf, sbuf);
  sprintf (sbuf, "RMSE Full = %8.3f \n", sqrt(sse_full/(ts_length-r-p)));
  strcat (lbuf, sbuf);
	    
  sprintf (sbuf, "F(Full Model) = %8.3f \n", freg);
  strcat (lbuf, sbuf);


  /*----- send address of lbuf back in what label points to -----*/
  *label = lbuf;
  
}
