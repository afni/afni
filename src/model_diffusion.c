/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  Routine to initilize and calculate the diffusion model, fitting
  for So and D, where S is acquired as a function of b:
  S(b)  = So exp(-b/D) where b is given in mm^2/sec

  File:     model_diffusion.c
  Author:   K. Donahue (modification of Doug Ward's programs)
  Date:     04 March 1998
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


  /*----- define interface for the diffusion model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "ADC");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 2;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "So");
  strcpy (mi->plabel[1], "D");

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =  0.0;    mi->max_constr[0] =   32256.0;
  mi->min_constr[1] =  0.00;    mi->max_constr[1] =   .004;


  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;


  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the "time" (b-value) series which results from using the
  diffusion signal model with the specified
  model parameters.

  Definition of model parameters:

     gs[0] = So  
     gs[1] = ADC: Apparent diffusion coefficient
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
  float t;                          /* time = in this case actually b factor */
  float fval;                       /* time series value at time t */  


  for (it = 0;  it < ts_length;  it++)
    {
      t = x_array[it][1];
      fval = gs[0] * exp(-t*gs[1]);
      ts_array[it] = fval;
    }
  
}




