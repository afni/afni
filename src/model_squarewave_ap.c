/*
  This file contains routines to initialize and implement the 
  square wave (amplitude and phase parameters) signal model.

  File:     model_squarewave_ap.c
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


  /*----- define interface for the square wave model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "SquareWave_AP");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 2;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "amplitude");
  strcpy (mi->plabel[1], "phase");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =   -100.0;    mi->max_constr[0] =   100.0;
  mi->min_constr[1] =    -90.0;    mi->max_constr[1] =     0.00;
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  square wave model with specified amplitude and phase parameters.

  Definition of model parameters:

	gs[0] = amplitude of square wave
	gs[1] = phase angle of square wave (degrees)
*/

void signal_model 
(
  float * gs,                /* parameters for signal model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated signal model time series */  
)

{
  const float FREQ = 0.125;         /* frequency of the square wave */
  int it;                           /* time index */     
  float t;                          /* time */
  float fval;                       /* time series value at time t */  


  /*----- calculate time series corresponding to the given parameters -----*/
  for (it = 0;  it < ts_length;  it++)
    { 
      t = x_array[it][1];
      fval = sin (2.0*PI*FREQ*t + (PI/180.0)*gs[1]);
      if (fval >= 0.0)
	fval = gs[0];
      else
	fval = -gs[0];
      ts_array[it] = fval;	
    }
  
}




