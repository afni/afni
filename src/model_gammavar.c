/*
  This file contains routines to initialize and implement the 
  gamma variate drug response signal model.

  File:     model_gammavar.c
  Author:   B. Douglas Ward
  Date:     29 May 1997
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

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;


  /*----- allocate memory space for model interface -----*/
  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));


  /*----- define interface for the gamma variate model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "GammaVar");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 4;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "t0");
  strcpy (mi->plabel[1], "k");
  strcpy (mi->plabel[2], "r");
  strcpy (mi->plabel[3], "b");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =    45.0;    mi->max_constr[0] =    75.0;
  mi->min_constr[1] =  -100.0;    mi->max_constr[1] =   100.0;
  mi->min_constr[2] =     1.0;    mi->max_constr[2] =    10.0;
  mi->min_constr[3] =     1.0;    mi->max_constr[3] =   100.0;
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  gamma variate drug response signal model with the specified
  model parameters.

  Definition of model parameters:

	 gs[0] = time delay of response (t0)  
	 gs[1] = multiplicative constant (k)
	 gs[2] = rise rate exponent (r)
	 gs[3] = decay rate constant (b)

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


  for (it = 0;  it < ts_length;  it++)
    {
      t = x_array[it][1];
      if (t < gs[0])
	fval = 0.0;
      else
	fval = gs[1] * exp( log(t-gs[0]) * gs[2] ) * exp(-(t-gs[0])/gs[3]);
      ts_array[it] = fval;
    }
  
}




