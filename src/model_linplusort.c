/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This file contains routines to initialize and implement the
  Linear+Ort noise model.

  File:     model_linplusort.c
  Author:   RW Cox
  Date:     24 Jul 2006

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

  /*----- define Linear+Ort noise model -----*/

  /*----- name of this model -----*/
  strcpy (mi->label, "Linear+Ort");

  /*----- this is a noise model -----*/
  mi->model_type = MODEL_NOISE_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 3;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "constant");
  strcpy (mi->plabel[1], "linear");
  strcpy (mi->plabel[2], "Ort");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] = -100.0;   mi->max_constr[0] = 100.0;
  mi->min_constr[1] =   -1.0;   mi->max_constr[1] =   1.0;
  mi->min_constr[2] =   -1.0;   mi->max_constr[2] =   1.0;

  /*----- function which implements the model -----*/
  mi->call_func = noise_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from the linear trend
  noise model and model parameters.

  Definition of model parameters:

     gn[0] = constant coefficient
     gn[1] = linear coefficient
     gn[2] = quadratic coefficient
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
  float t;                          /* time */
  float fval;                       /* time series value at time t */

  int ib = ts_length % 4 , nt = ts_length ;
  float g0=gn[0] , g1=gn[1] , g2=gn[2] ;

  switch( ib ){
    case 3: ts_array[2] = g1*x_array[2][1] + g0 + g2*x_array[2][2]; /* fall thru */
    case 2: ts_array[1] = g1*x_array[1][1] + g0 + g2*x_array[1][2]; /* fall thru */
    case 1: ts_array[0] = g1*x_array[0][1] + g0 + g2*x_array[0][2]; break ;
  }
  for( it=ib ; it < nt ; it+=4 ){
    ts_array[it  ] = g1*x_array[it  ][1] + g0 + g2*x_array[it  ][2];
    ts_array[it+1] = g1*x_array[it+1][1] + g0 + g2*x_array[it+1][2];
    ts_array[it+2] = g1*x_array[it+2][1] + g0 + g2*x_array[it+2][2];
    ts_array[it+3] = g1*x_array[it+3][1] + g0 + g2*x_array[it+3][2];
  }
}
