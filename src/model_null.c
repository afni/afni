/*
  This file contains routines to initialize and implement the 
  null signal model.

  File:     model_null.c
  Author:   B. Douglas Ward
  Date:     22 May 1997
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


  /*----- define interface for the null signal model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "Null");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 0;

  /*----- minimum and maximum parameter constraints -----*/
  /*----- there are none -----*/
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  null signal model.
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


  for (it = 0;  it < ts_length;  it++)
    ts_array[it] = 0.0;
  
}




