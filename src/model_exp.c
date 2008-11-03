/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the 
  differential exponential drug response signal model.

  File:     model_exp.c
  Author:   Z. Saad, based on model_diffexp.c by B. Douglas Ward
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
  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));


  /*----- define interface for the differential - exponential model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "Exp");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 2;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "a");
  strcpy (mi->plabel[1], "b");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =  -500.0;    mi->max_constr[0] =   500.0;
  mi->min_constr[1] =   1.00;   mi->max_constr[1] =     1.00;
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  an exponential signal model with the specified
  model parameters.

  Definition of model parameters:

     gs[0] = multiplicative constant (k)
     gs[1] = elimination rate constant (alpha1)
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
	fval = gs[0] * (exp(gs[1]*(t)));
      ts_array[it] = fval;
    }
  
}




