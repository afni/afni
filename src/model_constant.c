/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the
  constant plus noise model.

  File:     model_constant.c
  Author:   B. Douglas Ward
  Date:     29 May 1997

*/


/*---------------------------------------------------------------------------*/

#include <math.h>
#include "NLfit_model.h"


void noise_model 
(
  float * gn,                /* parameters for noise model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated noise model time series */  
);


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the noise model by defining the number of parameters
  in the noise model, the name of the noise model, and the default values
  for the minimum and maximum parameter constraints.
*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;


  /*----- allocate memory space for model interface -----*/
  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));


  /*----- define constant plus noise model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "Constant");

  /*----- this is a noise model -----*/
  mi->model_type = MODEL_NOISE_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 1;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "constant");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] = -100.0;   mi->max_constr[0] = 100.0;
  
  /*----- function which implements the model -----*/
  mi->call_func = &noise_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from the constant plus
  noise model and model parameters.

  Definition of model parameters:

     gn[0] = constant coefficient
*/

void noise_model 
(
  float * gn,                /* parameters for noise model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated noise model time series */  
)

{
  int it;                           /* time index */     
  float fval;                       /* time series value at time t */  


  for (it = 0;  it < ts_length;  it++)
    {
      fval = gn[0];
      ts_array[it] = fval;
    }
  
}

