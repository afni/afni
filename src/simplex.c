/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This file implements the Simplex algorithm for non-linear optimization.
   Some routines are adapted from:  A Jump Start Course in C++ Programming

  File:     simplex.c
  Author:   B. Douglas Ward
  Date:     23 May 1997

  Mod:      Changed random number generator function from rand to drand48.
            29 August 1997
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
/*
  Routine to generate a uniform U(0,1) random variate.
*/

float uniform ()
{
  return ( (float)drand48() );
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a normal N(0,1) random variate.
*/

void normal (float * n1, float * n2)
{
  float u1, u2;
  float r;


  u1 = 0.0;
  while (u1 <= 0.0)
    {
      u1 = uniform();
    }
  u2 = uniform();

  r   = sqrt(-2.0*log(u1));
  *n1 = r * cos(2.0*PI*u2);
  *n2 = r * sin(2.0*PI*u2);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a U(a,b) random variate.
*/

float get_random_value(float a, float b)
{
  float fval;

  fval = a + uniform() * (b-a);

  return (fval);
}


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
)

{
  int i;

  *centroid = (float *) malloc (sizeof(float) * dimension);
  *step_size = (float *) malloc (sizeof(float) * dimension);
  *test1 = (float *) malloc (sizeof(float) * dimension);
  *test2 = (float *) malloc (sizeof(float) * dimension);

  *response = (float *) malloc (sizeof(float) * (dimension+1));   
  *simplex = (float **) malloc (sizeof(float *) * (dimension+1));

  for (i = 0;  i < dimension+1;  i++)
    (*simplex)[i] = (float *) malloc (sizeof(float) * dimension);
       
}


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
)

{
  int i, j;
  float minval, maxval;


  /*----- copy parameter vector into first vertex of simplex -----*/
  for (i = 0;  i < dimension;  i++)    
    simplex[0][i] = parameters[i];

    
  /*----- set up initial step sizes -----*/
  for (i = 0;  i < r;  i++)
    step_size[i] = 0.1 * (max_nconstr[i] - min_nconstr[i]);
  for (i = r;  i < dimension;  i++)
    step_size[i] = 0.1 * (max_sconstr[i-r] - min_sconstr[i-r]);

    
  /*----- choose random vectors for remaining vertices -----*/
  for (i = 1;  i < dimension+1;  i++)
    {
      /*----- choose noise model parameters -----*/
      for (j = 0;  j < r;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  if (nabs)   /*--- absolute noise parameter constraints ---*/
	    {
	      if (minval < min_nconstr[j])  
		minval = min_nconstr[j];
	    }
	  else        /*--- relative noise parameter constraints ---*/
	    {
	      if (minval < min_nconstr[j] + par_rdcd[j])  
		minval = min_nconstr[j] + par_rdcd[j];
	    }

	  maxval = simplex[0][j] + step_size[j];
	  if (nabs)   /*--- absolute noise parameter constraints ---*/
	    {
	      if (maxval > max_nconstr[j])  
		maxval = max_nconstr[j];
	    }
	  else        /*--- relative noise parameter constraints ---*/
	    {
	      if (maxval > max_nconstr[j] + par_rdcd[j])  
		maxval = max_nconstr[j] + par_rdcd[j];
	    }
	  simplex[i][j] = get_random_value (minval, maxval);
	}


      /*----- choose signal model parameters -----*/
      for (j = r;  j < dimension;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  if (minval < min_sconstr[j-r])  
	    minval = min_sconstr[j-r];
	  maxval = simplex[0][j] + step_size[j];
	  if (maxval > max_sconstr[j-r])  
	    maxval = max_sconstr[j-r];
	  simplex[i][j] = get_random_value (minval, maxval);
	}
    }


  /*----- calculate and save sse for each vertex of simplex -----*/
  for (i = 0;  i < dimension+1;  i++)
    response[i] = calc_sse(nmodel, smodel, r, p, nabs, 
			   min_nconstr, max_nconstr, min_sconstr, max_sconstr,
			   par_rdcd, simplex[i], ts_length, x_array, ts_array);

}


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
)

{
  int i;

  /* initialize values */
  *worst = 0;
  *best = 0;

  /* find the best and worst */
  for (i = 1;  i < dimension+1;  i++)
    {
      if (response[i] > response[*worst])
	*worst = i;
      if (response[i] < response[*best])
	*best = i;
    }

  /* find the next worst index */
  if (*worst == 0)
    *next = 1;
  else
    *next = 0;
  
  for (i = 0;  i < dimension+1;  i++)
    if ((i != *worst) && (response[i] > response[*next]))
      *next = i;
}


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
)

{
  const float STEP_FACTOR = 0.9;
  int i, j;
  int worst, next, best;
  float minval, maxval;


  /* find the current best vertex */
  eval_vertices (dimension, response, &worst, &next, &best); 


  /* set the first vertex to the current best */
  for (i = 0; i < dimension;  i++)
    simplex[0][i] = simplex[best][i];


  /* decrease step size */
  for (i = 0;  i < dimension;  i++)
    step_size[i] *= STEP_FACTOR;


  /* set up remaining vertices of simplex using new step size */
  for (i = 1;  i < dimension+1;  i++)
    {
      /*----- choose noise model parameters -----*/
      for (j = 0;  j < r;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  if (nabs)   /*--- absolute noise parameter constraints ---*/
	    {
	      if (minval < min_nconstr[j])  
		minval = min_nconstr[j];
	    }
	  else        /*--- relative noise parameter constraints ---*/
	    {
	      if (minval < min_nconstr[j] + par_rdcd[j])  
		minval = min_nconstr[j] + par_rdcd[j];
	    }

	  maxval = simplex[0][j] + step_size[j];
	  if (nabs)
	    {
	      if (maxval > max_nconstr[j])  
		maxval = max_nconstr[j];
	    }
	  else
	    {
	      if (maxval > max_nconstr[j] + par_rdcd[j])  
		maxval = max_nconstr[j] + par_rdcd[j];
	    }

	  simplex[i][j] = get_random_value (minval, maxval);
	}


      /*----- choose signal model parameters -----*/
      for (j = r;  j < dimension;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  if (minval < min_sconstr[j-r])  
	    minval = min_sconstr[j-r];
	  maxval = simplex[0][j] + step_size[j];
	  if (maxval > max_sconstr[j-r])  
	    maxval = max_sconstr[j-r];
	  simplex[i][j] = get_random_value (minval, maxval);
	}
    }

  
  /* initialize response for each vector */
  for (i = 0;  i < dimension+1;  i++)
    response[i] = calc_sse (nmodel, smodel, r, p, nabs, 
			   min_nconstr, max_nconstr, min_sconstr, max_sconstr,
			   par_rdcd, simplex[i], ts_length, x_array, ts_array);
}


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
)

{
  int i, j;

  for (i = 0;  i < dimension;  i++)
    {
      centroid[i] = 0.0;

      /* add each vertex, except the worst */
      for (j = 0;  j < dimension+1;  j++)
	if (j != worst)
	  centroid[i] += simplex[j][i];
    }

  /* divide by the number of vertices */
  for (i = 0;  i < dimension;  i++)
    centroid[i] /= dimension;
}


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
)

{
  int i;

  for (i = 0;  i < dimension;  i++)
    vertex[i] = centroid[i] + coef*(centroid[i] - simplex[worst][i]);
}


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
)

{
  int i;

  for (i = 0;  i < dimension;  i++)
    simplex[index][i] = vertex[i];

  response[index] = resp;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the goodness of fit.  This is measured by the variation in 
  responses at the different vertices relative to the average response.
*/

float calc_good_fit 
(
  int dimension,                   /* dimension of parameter space */
  float * response                 /* error sum of squares at each vertex */
)

{
  int i;

  float avg, sd, tmp;

  /*----- average the response values -----*/
  avg = 0.0;
  for (i = 0;  i < dimension+1;  i++)
    avg += response[i];
  avg /= dimension+1;

  /*----- compute standard deviation of response -----*/
  sd = 0.0;
  for (i = 0;  i < dimension+1;  i++)
    {
      tmp = response[i] - avg;
      sd += tmp*tmp;
    }

  sd /= dimension;

  return (sqrt(sd) / avg);
}


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
)

{
  int iv;                     /* vertex index */   


  free (*centroid);    *centroid = NULL;
  free (*response);    *response = NULL;
  free (*step_size);   *step_size = NULL;
  free (*test1);       *test1 = NULL;
  free (*test2);       *test2 = NULL;

  for (iv = 0;  iv < dimension+1;  iv++)
    {
      free ((*simplex)[iv]);
      (*simplex)[iv] = NULL;
    }

  free (*simplex);     *simplex = NULL;
       
}


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
)

{
  const int MAX_ITERATIONS = 50;         /* maximum number of iterations */
  const int MAX_RESTARTS = 5;            /* maximum number of restarts */
  const float EXPANSION_COEF = 2.0;      /* expansion coefficient */
  const float REFLECTION_COEF = 1.0;     /* reflection coefficient */
  const float CONTRACTION_COEF = 0.5;    /* contraction coefficient */
  const float TOLERANCE = 1.0e-4;        /* solution convergence tolerance */

  float ** simplex   = NULL;    /* the simplex itself */
  float * centroid   = NULL;    /* center of mass of the simplex */
  float * response   = NULL;    /* error sum of squares at each vertex */
  float * step_size  = NULL;    /* controls random placement of new vertex */
  float * test1      = NULL;    /* test vertex */
  float * test2      = NULL;    /* test vertex */
  float resp1, resp2;           /* error sum of squares for test vertex */
  int i;                        /* vertex index */ 
  int worst;                    /* index of worst vertex in simplex */
  int best;                     /* index of best vertex in simplex */
  int next;                     /* index of next-to-worst vertex in simplex */ 
  int num_iter;                 /* number of simplex algorithm iterations */
  int num_restarts;             /* number of restarts of simplex algorithm */
  int done;                     /* boolean for search finished */
  float fit;                    /* array of fitted time series values */
  int dimension;                /* dimension of parameter space */


  /*----- dimension of parameter space -----*/
  dimension = r + p;

  /*----- allocate memory -----*/
  allocate_arrays (dimension, &simplex, &centroid, &response, &step_size,
		   &test1, &test2);
  
  /*----- initialization for simplex algorithm -----*/
  initialize_simplex (dimension, nmodel, smodel, r, p, nabs,
		      min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		      par_rdcd, parameters, simplex, response, step_size, 
		      ts_length, x_array, ts_array);

  /* start loop to do simplex optimization */
  num_iter = 0;
  num_restarts = 0;
  done = 0;
  
  while (!done)
    {
      /*----- find the worst vertex and compute centroid of remaining simplex, 
	discarding the worst vertex -----*/
      eval_vertices (dimension, response, &worst, &next, &best);
      calc_centroid (dimension, simplex, worst, centroid);
      
      /*----- reflect the worst point through the centroid -----*/
      calc_reflection (dimension, simplex, centroid, worst, 
		       REFLECTION_COEF, test1);
      resp1 = calc_sse (nmodel, smodel, r, p, nabs, min_nconstr, max_nconstr, 
			min_sconstr, max_sconstr, par_rdcd, test1, 
			ts_length, x_array, ts_array);


      /*----- test the reflection against the best vertex and expand it if the
	reflection is better.  if not, keep the reflection -----*/
      if (resp1 < response[best])
	{
	  /*----- try expanding -----*/
	  calc_reflection (dimension, simplex, centroid, worst, 
			   EXPANSION_COEF, test2);

	  resp2 = calc_sse (nmodel, smodel, r, p, nabs, 
			    min_nconstr, max_nconstr, 
			    min_sconstr, max_sconstr, par_rdcd, test2, 
			    ts_length, x_array, ts_array);

	  if (resp2 <= resp1)    /* keep expansion */     
	    replace (dimension, simplex, response, worst, test2, resp2);
	  else                   /* keep reflection */
	    replace (dimension, simplex, response, worst, test1, resp1);
	}

      else if (resp1 < response[next])
	{
	  /*----- new response is between the best and next worst 
	    so keep reflection -----*/
	  replace (dimension, simplex, response, worst, test1, resp1); 
	}
          else
	{
	  /*----- try contraction -----*/
	  if (resp1 >= response[worst])
	    calc_reflection (dimension, simplex, centroid, worst, 
			     -CONTRACTION_COEF, test2);
	  else
	    calc_reflection (dimension, simplex, centroid, worst, 
			     CONTRACTION_COEF, test2);

	  resp2 = calc_sse (nmodel, smodel, r, p, nabs, 
			    min_nconstr, max_nconstr, 
			    min_sconstr, max_sconstr, par_rdcd, test2, 
			    ts_length, x_array, ts_array);
	  
	  /*---- test the contracted response against the worst response ----*/
	  if (resp2 > response[worst])
	    {
	      /*----- new contracted response is worse, so decrease step size
		and restart -----*/
	      num_iter = 0;
	      num_restarts += 1;
	      restart (dimension, nmodel, smodel, r, p, nabs,
		       min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		       par_rdcd, simplex, response, step_size, 
		       ts_length, x_array, ts_array);
	    }
	  else       /*----- keep contraction -----*/
	    replace (dimension, simplex, response, worst, test2, resp2);
	}

      /*----- test to determine when to stop.  
	first, check the number of iterations -----*/
      num_iter += 1;    /*----- increment iteration counter -----*/
      if (num_iter >= MAX_ITERATIONS)
	{
	  /*----- restart with smaller steps -----*/
	  num_iter = 0;
	  num_restarts += 1;
	  restart (dimension, nmodel, smodel, r, p, nabs,
		   min_nconstr, max_nconstr, min_sconstr, max_sconstr, 
		   par_rdcd, simplex, response, step_size, 
		   ts_length, x_array, ts_array);
	}

      /*----- limit the number of restarts -----*/
      if (num_restarts == MAX_RESTARTS)  done = 1;

      /*----- compare relative standard deviation of vertex responses 
	against a defined tolerance limit -----*/
      fit = calc_good_fit (dimension, response);
      if (fit <= TOLERANCE)  done = 1;

      /*----- if done, copy the best solution to the output array -----*/
      if (done) 
	{
	  eval_vertices (dimension, response, &worst, &next, &best);
	  for (i = 0;  i < dimension;  i++)
	    parameters[i] = simplex[best][i];
	  *sse = response[best];
	}

    }  /*----- while (!done) -----*/
 
  deallocate_arrays (dimension, &simplex, &centroid, &response, &step_size,
		     &test1, &test2);

}



















