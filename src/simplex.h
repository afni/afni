/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This is the header file for simplex.c.

  File:     simplex.h
  Author:   B. Douglas Ward
  Date:     23 May 1997

*/

/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/


/*---------------------------------------------------------------------------*/

#include "NLfit_model.h"

/*---------------------------------------------------------------------------*/
/*
  Routine to generate a uniform U(0,1) random variate.
*/

float uniform ();


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a normal N(0,1) random variate.
*/

void normal (float * n1, float * n2);


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a U(a,b) random variate.
*/

float get_random_value(float a, float b);


/*---------------------------------------------------------------------------*/
/*
  Allocate memory for simplex algorithm.
*/

void allocate_arrays 
(
  int dimension,              /* dimension of parameter space */
  float *** simplex,          /* the simplex itself */
  float ** centroid,          /* center of mass of the simplex */
  float ** response,          /* error sum of squares at each vertex */
  float ** step_size,         /* controls random placement of new vertex */
  float ** test1,             /* test vertex */
  float ** test2              /* test vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Set up initial values for the simplex vertices.
*/

void initialize_simplex 
(
  int dimension,          /* dimension of the full model */
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  float * parameters,     /* starting point */
  float ** simplex,       /* the simplex itself */
  float * response,       /* sse at each vertex of the simplex */
  float * step_size,      /* amount of allowed variation at each parameter */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array        /* observed time series */
);


/*---------------------------------------------------------------------------*/
/*
  Evaluate the vertices of the simplex.  Find indices of the best, worst, and 
  next-to-worst vertices.
*/

void eval_vertices 
(
 int dimension,            /* dimension of parameter space */
 float * response,         /* error sum of squares at each vertex */
 int * worst,              /* index of worst vertex in simplex */
 int * next,               /* index of next-to-worst vertex in simplex */
 int * best                /* index of best vertex in simplex */
);


/*---------------------------------------------------------------------------*/
/*
  Routine to restart the simplex algorithm by putting random vectors into
  the simplex (and keeping the best previous vertex).
*/

void restart 
(
  int dimension,          /* dimension of the full model */
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  float ** simplex,       /* the simplex itself */
  float * response,       /* sse at each vertex of the simplex */
  float * step_size,      /* amount of allowed variation at each parameter */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array        /* observed time series */
);


/*---------------------------------------------------------------------------*/
/*
  Calculate the centroid of the simplex, ignoring the worst vertex.
*/

void calc_centroid 
(
  int dimension,         /* dimension of parameter space */
  float ** simplex,      /* the simplex itself */
  int worst,             /* index of worst vertex in simplex */
  float * centroid       /* center of mass of the simplex minus worst vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Calculate the reflection of the worst vertex about the centroid.
*/

void calc_reflection 
(
  int dimension,               /* dimension of parameter space */
  float ** simplex,            /* the simplex itself */
  float * centroid,            /* center of mass of the simplex */
  int worst,                   /* index of worst vertex in simplex */
  float coef,                  /* expansion or contraction factor */
  float * vertex               /* new vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Replace a vertex of the simplex.
*/

void replace 
(
  int dimension,              /* dimension of parameter space */
  float ** simplex,           /* the simplex itself */
  float * response,           /* error sum of squares at each vertex */
  int index,                  /* index of vertex to be replaced */
  float * vertex,             /* new vertex */
  float resp                  /* error sum of squares at new vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Calculate the goodness of fit.  This is measured by the variation in 
  responses at the different vertices relative to the average response.
*/

float calc_good_fit 
(
  int dimension,                   /* dimension of parameter space */
  float * response                 /* error sum of squares at each vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Release memory required for simplex optimization.
*/

void deallocate_arrays 
(
  int dimension,              /* dimension of parameter space */
  float *** simplex,          /* the simplex itself */
  float ** centroid,          /* center of mass of the simplex */
  float ** response,          /* error sum of squares at each vertex */
  float ** step_size,         /* controls random placement of new vertex */
  float ** test1,             /* test vertex */
  float ** test2              /* test vertex */
);


/*---------------------------------------------------------------------------*/
/*
  Implementation of the (non-linear) simplex optimization algorithm.
*/

void simplex_optimization
( 
  vfp nmodel,             /* pointer to noise model */
  vfp smodel,             /* pointer to signal model */
  int r,                  /* number of parameters in the noise model */
  int p,                  /* number of parameters in the signal model */
  float * min_nconstr,    /* minimum parameter constraints for noise model */
  float * max_nconstr,    /* maximum parameter constraints for noise model */
  float * min_sconstr,    /* minimum parameter constraints for signal model */
  float * max_sconstr,    /* maximum parameter constraints for signal model */
  int nabs,               /* use absolute constraints for noise parameters */
  int ts_length,          /* length of time series array */
  float ** x_array,       /* independent variable matrix */
  float * ts_array,       /* observed time series */
  float * par_rdcd,       /* estimated parameters for the reduced model */
  float * parameters,     /* estimated parameters */
  float * sse             /* error sum of squares */
);


/*---------------------------------------------------------------------------*/

















