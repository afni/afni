/*
  This file contains routines to initialize and implement the 
  differential exponential drug response signal model.

  File:     model_diffexp.c
  Author:   B. Douglas Ward
  Date:     6 June 1997
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


  /*----- define interface for the differential - exponential model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "DiffExp");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 4;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "t0");
  strcpy (mi->plabel[1], "k");
  strcpy (mi->plabel[2], "alpha1");
  strcpy (mi->plabel[3], "alpha2");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =    45.0;    mi->max_constr[0] =    75.0;
  mi->min_constr[1] =  -500.0;    mi->max_constr[1] =   500.0;
  mi->min_constr[2] =     0.00;   mi->max_constr[2] =     0.15;
  mi->min_constr[3] =     0.15;   mi->max_constr[3] =     0.50;
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


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
     gs[1] = multiplicative constant (k)
     gs[2] = elimination rate constant (alpha1)
     gs[3] = absorption rate constant (alpha2)
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
	fval = gs[1] * (exp(-gs[2]*(t-gs[0])) - exp(-gs[3]*(t-gs[0])));
      ts_array[it] = fval;
    }
  
}




