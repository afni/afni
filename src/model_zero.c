/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This file contains routines to initialize and implement the
  zero noise model.

Copied from model_null.c, the null signal model
  and from model_constant.c, the constant noise model
  File:     model_zero.c
  Author:   Daniel Glen
  Date:     26 Oct 2006
*/


/*---------------------------------------------------------------------------*/

/*#include <math.h>*/
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


  /*----- define interface for the zero noise model -----*/

  /*----- name of this model -----*/
  strcpy (mi->label, "Zero");

  /*----- this is a noise model -----*/
  mi->model_type = MODEL_NOISE_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 0;

  /*----- minimum and maximum parameter constraints -----*/
  /*----- there are none -----*/

  /*----- function which implements the model -----*/
  mi->call_func = &noise_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  zero noise model.
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


  for (it = 0;  it < ts_length;  it++)
    ts_array[it] = 0.0;

}




