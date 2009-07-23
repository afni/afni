/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*
  This file contains routines to initialize and implement the 
  differential exponential signal model for MEMRI applications

  File:     model_expMEMRI2.c
  Author:   D. Glen based on model_exp.c by Z.Saad and
            model_diffexp.c by B. Douglas Ward
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

/*static float *rt;
static int rt_len;*/

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
  MRI_IMAGE * im;
  char      * envp;
  int ii;

  /*----- allocate memory space for model interface -----*/
  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));


  /*----- define interface for the differential - exponential model -----*/   

  /*----- name of this model -----*/
  strcpy (mi->label, "expMEMRI3");

  /*----- this is a signal model -----*/
  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/
  mi->params = 5;

  /*----- parameter labels -----*/
  strcpy (mi->plabel[0], "a - wash-in delay");
  strcpy (mi->plabel[1], "b - wash-in rate");
  strcpy (mi->plabel[2], "c - wash-in scalefactor");
  strcpy (mi->plabel[3], "d - wash-out delay");
  strcpy (mi->plabel[4], "e - wash-out rate");
/*  strcpy (mi->plabel[5], "f - wash-out scalefactor");*/

  /*----- minimum and maximum parameter constraints -----*/
  mi->min_constr[0] =  0.0;    mi->max_constr[0] =   100.0;
  mi->min_constr[1] =   -100.00;   mi->max_constr[1] =     0.00;
  mi->min_constr[2] = 0.0;  mi->max_constr[2] = 100.0;
  mi->min_constr[3] =  0.0;    mi->max_constr[3] =   500.0;
  mi->min_constr[4] =   -100.00;   mi->max_constr[4] =     0.00;
/*  mi->min_constr[5] = 0.0;  mi->max_constr[5] = 100.0;*/
 
  /*----- function which implements the model -----*/
  mi->call_func = &signal_model;

#if 0
  envp = my_getenv("AFNI_MODEL_EXPMEMRI_T_FILE");
  if( !envp )
  {
      fprintf(stderr,"\n** NLfim: need env var AFNI_EXPMEMRI_T_FILE\n");
      fprintf(stderr,"    (a 1D file of time values for each sub-brick)\n");
      return(NULL);
  }

  im = mri_read_1D(envp);
  if( !im )
  {
      fprintf(stderr,"** failed to open time file %s for model\n", envp);
      return(NULL);
  }

  /* nx == 1 and ny > 1, take the transpose */
  if( im->nx == 1 && im->ny > 1 )
  {
      MRI_IMAGE * flim = mri_transpose(im);
      mri_free(im);
      im = flim;
      if( !im ) { fprintf(stderr,"** time transposing failure\n"); return(NULL); }
      fprintf(stderr,"taking transpose of time file, new len = %d\n",im->nx);
  }

  rt = MRI_FLOAT_PTR(im);        /* do not free this */
  rt_len = im->nx;
  fprintf(stderr,"Times at each sub-brick :\n");
  for(ii=0;ii<im->nx;ii++)
    fprintf(stderr, "%f  ", rt[ii]);
  fprintf(stderr, "\n");

#endif

  /*----- return pointer to the model interface -----*/
  return (mi);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  an exponential signal model with the specified
  model parameters.

  Definition of model parameters:

  y = c / (1 + exp(a+bt)) - c/(1+exp(d+et)) (baseline in noise model +g)

gs[0] = a
gs[1] = b

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
      fval = gs[2] / (1.0+exp(gs[0] + gs[1]*t)) - \
             gs[2] / (1.0+exp(gs[3] + gs[4]*t));
      ts_array[it] = fval;
    }
  
}




