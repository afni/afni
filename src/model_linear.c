/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the
  linear trend noise model.

  File:     model_linear.c
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
  mi = (MODEL_interface *) RwcMalloc (sizeof(MODEL_interface));


  /*----- define linear plus noise model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "Linear");

  /*----- this is a noise model -----*/
  mi->model_type = MODEL_NOISE_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 2;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "constant");
  strcpy (mi->plabel[1], "linear");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] = -100.0;   mi->max_constr[0] = 100.0;
  mi->min_constr[1] =   -1.0;   mi->max_constr[1] =   1.0;
  
  /*----- function which implements the model -----*/
  mi->call_func = (void_func *)noise_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}

#define UNROLL

/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from the linear trend
  noise model and model parameters.

  Definition of model parameters:

     gn[0] = constant coefficient
     gn[1] = linear coefficient
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


#ifdef UNROLL
  { int ib = ts_length % 4 , nt = ts_length ;
    float g0=gn[0] , g1=gn[1] ;
    switch( ib ){
      case 3: ts_array[2] = g1*x_array[2][1] + g0 ; /* fall thru */
      case 2: ts_array[1] = g1*x_array[1][1] + g0 ; /* fall thru */
      case 1: ts_array[0] = g1*x_array[0][1] + g0 ; break ;
    }
    for( it=ib ; it < nt ; it+=4 ){
      ts_array[it  ] = g1*x_array[it  ][1] + g0 ;
      ts_array[it+1] = g1*x_array[it+1][1] + g0 ;
      ts_array[it+2] = g1*x_array[it+2][1] + g0 ;
      ts_array[it+3] = g1*x_array[it+3][1] + g0 ;
    }
  }
#else
  for (it = 0;  it < ts_length;  it++)
    {
      t = x_array[it][1];
      fval = gn[0] + gn[1]*t;
      ts_array[it] = fval;
    }
#endif
  
}
