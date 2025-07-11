/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the 
  beta distribution function signal model.

  File:     model_beta.c
  Author:   B. Douglas Ward
  Date:     11 December 1997
*/


/*---------------------------------------------------------------------------*/

#include <math.h>
#include "NLfit_model.h"

void signal_model 
(
  float * gs,                /* parameters for signal model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated signal model time series */  
);


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the signal model by defining the number of parameters
  in the signal model, the name of the signal model, and the default values
  for the minimum and maximum parameter constraints.
*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;


  /*----- allocate memory space for model interface -----*/
  mi = (MODEL_interface *) RwcMalloc (sizeof(MODEL_interface));


  /*----- define interface for the differential - exponential model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "Beta");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 5;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "t0");
  strcpy (mi->plabel[1], "tf");
  strcpy (mi->plabel[2], "k");
  strcpy (mi->plabel[3], "alpha");
  strcpy (mi->plabel[4], "beta");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =    27.0;    mi->max_constr[0] =    77.0;
  mi->min_constr[1] =    78.0;    mi->max_constr[1] =   250.0;
  mi->min_constr[2] = -5000.0;    mi->max_constr[2] =  5000.0;
  mi->min_constr[3] =    0.00;    mi->max_constr[3] =    10.0;
  mi->min_constr[4] =    0.00;    mi->max_constr[4] =    10.0;
  
  /*----- function which implements the model -----*/
  mi->call_func = (void_func *)&signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  differential exponential drug response signal model with the specified
  model parameters.

  Definition of model parameters:

     gs[0] = time delay of response (t0)  
     gs[1] = end of response (tf)
     gs[2] = multiplicative constant (k)
     gs[3] = beta distribution parameter (alpha)
     gs[4] = beta distribution parameter (beta)
*/

void signal_model 
(
  float * gs,                /* parameters for signal model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated signal model time series */  
)

{
  int it;                           /* time index */     
  float t;                          /* time */
  float fval;                       /* time series value at time t */  
  float x;                          /* rescaled t */
  float t0, tf, k, alpha, beta;     /* parameters */


  t0 = gs[0];
  tf = gs[1];
  k  = gs[2];
  alpha = gs[3];
  beta  = gs[4];

  for (it = 0;  it < ts_length;  it++)
    {
      t = x_array[it][1];
      if ((t <= t0) || (t >= tf) || (t0 >= tf))
	fval = 0.0;
      else
	{
	  x = (t - t0) / (tf - t0);
	  fval = k * exp((alpha-1.0)*log(x)) * exp((beta-1.0)*log(1.0-x));
	}
      ts_array[it] = fval;
    }
}
  





