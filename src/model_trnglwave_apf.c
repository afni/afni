/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the 
  triangular wave (amplitude, phase, and frequency parameters) signal model.

  File:     model_trnglrwave_apf.c
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


  /*----- define interface for the triangular wave model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "TrnglrWave_APF");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 3;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "amplitude");
  strcpy (mi->plabel[1], "phase");
  strcpy (mi->plabel[2], "frequency");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =   -100.0;    mi->max_constr[0] =   100.0;
  mi->min_constr[1] =    -90.0;    mi->max_constr[1] =     0.00;
  mi->min_constr[2] =      0.1;    mi->max_constr[2] =     0.15;
  
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  triangular wave model with specified amplitude, phase, and frequency
  model parameters.

  Definition of model parameters:

	gs[0] = amplitude of triangular wave
	gs[1] = phase angle of triangular wave (degrees)
	gs[2] = frequency of triangular wave (cycles / time_step)
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
  float cycles;                     /* number of cycles since initial time */


  /*----- calculate time series corresponding to the given parameters -----*/
  for (it = 0;  it < ts_length;  it++)
    {
      t = x_array[it][1];
      cycles = gs[2]*t + gs[1]/360.0;
      cycles = cycles - (int)cycles;
      if (cycles <= 0.25)
	fval = gs[0] * (cycles/0.25);
      else
	if ((cycles > 0.25) && (cycles <= 0.75))
	  fval = gs[0] * (2.0 - (cycles/0.25));
	else
	  fval = gs[0] * ((cycles/0.25) - 4.0);
      ts_array[it] = fval;	
    }
}




