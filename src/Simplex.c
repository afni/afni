/*---------------------------------------------------------------------------*/
/*
  This file contains routines for implementing the Simplex algorithm for 
  non-linear optimization to estimate the unknown parameters.

  The Simplex algorithm is adapted from: A Jump Start Course in C++ Programming
  
  File:    Simplex.c
  Author:  B. Douglas Ward
  Date:    28 January 2000


  Note:  The calling program should include the following statements
         prior to "#include "Simplex.c"

  #define DIMENSION p    where p = number of parameters to be estimated
  #include "randgen.c"
  float calc_error (float * vertex);


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
*/


/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

int count = 0;               /* count of error evaluations */
int number_restarts = 0;     /* count of simplex algorithm restarts */


/*---------------------------------------------------------------------------*/

void allocate_arrays (float *** simplex, float ** centroid,
		      float ** response, float ** step_size, 
		      float ** test1, float ** test2)
{
  int i;

  *centroid = (float *) malloc (sizeof(float) * DIMENSION);
  *response = (float *) malloc (sizeof(float) * (DIMENSION+1));
  *step_size = (float *) malloc (sizeof(float) * DIMENSION);
  *test1 = (float *) malloc (sizeof(float) * DIMENSION);
  *test2 = (float *) malloc (sizeof(float) * DIMENSION);
   
  *simplex = (float **) malloc (sizeof(float *) * (DIMENSION+1));

  for (i = 0;  i < DIMENSION+1;  i++)
    (*simplex)[i] = (float *) malloc (sizeof(float) * DIMENSION);
       
}


/*---------------------------------------------------------------------------*/

void deallocate_arrays (float *** simplex, float ** centroid,
			float ** response, float ** step_size, 
			float ** test1, float ** test2)
{
  int i;

  free (*centroid);    *centroid = NULL;
  free (*response);    *response = NULL;
  free (*step_size);   *step_size = NULL;
  free (*test1);       *test1 = NULL;
  free (*test2);       *test2 = NULL;

  for (i = 0;  i < DIMENSION+1;  i++)
    {
      free ((*simplex)[i]);
      (*simplex)[i] = NULL;
    }

  free (*simplex);     *simplex = NULL;
       
}


/*---------------------------------------------------------------------------*/

void eval_vertices (float * response, int * worst, int * next, int * best)
{
  int i;

  /* initialize values */
  *worst = 0;
  *best = 0;

  /* find the best and worst */
  for (i = 1;  i < DIMENSION+1;  i++)
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
  
  for (i = 0;  i < DIMENSION+1;  i++)
    if ((i != *worst) && (response[i] > response[*next]))
      *next = i;
}


/*---------------------------------------------------------------------------*/

void restart (float ** simplex, float * response, 
	      float * step_size)
{
  const float STEP_FACTOR = 0.9;
  int i, j;
  int worst, next, best;
  float minval, maxval;


  /* find the current best vertex */
  eval_vertices (response, &worst, &next, &best); 

  /* set the first vertex to the current best */
  for (i = 0; i < DIMENSION;  i++)
    simplex[0][i] = simplex[best][i];

  /* decrease step size */
  for (i = 0;  i < DIMENSION;  i++)
    step_size[i] *= STEP_FACTOR;

  /* set up remaining vertices of simplex using new step size */
  for (i = 1;  i < DIMENSION+1;  i++)
    for (j = 0;  j < DIMENSION;  j++)
      {
	minval = simplex[0][j] - step_size[j];
	maxval = simplex[0][j] + step_size[j];
      simplex[i][j] = rand_uniform (minval, maxval);
      }

  /* initialize response for each vector */
  for (i = 0;  i < DIMENSION+1;  i++)
    response[i] = calc_error (simplex[i]);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the centroid of the simplex, ignoring the worst vertex.
*/

void calc_centroid (float ** simplex, int worst, float * centroid)
{
  int i, j;

  for (i = 0;  i < DIMENSION;  i++)
    {
      centroid[i] = 0.0;

      /* add each vertex, except the worst */
      for (j = 0;  j < DIMENSION+1;  j++)
	if (j != worst)
	  centroid[i] += simplex[j][i];
    }

  /* divide by the number of vertices */
  for (i = 0;  i < DIMENSION;  i++)
    centroid[i] /= DIMENSION;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the reflection of the worst vertex about the centroid.
*/

void calc_reflection (float ** simplex, float * centroid, 
		      int worst, float coef, float * vertex)
{
  int i;

  for (i = 0;  i < DIMENSION;  i++)
    vertex[i] = centroid[i] + coef*(centroid[i] - simplex[worst][i]);
}


/*---------------------------------------------------------------------------*/
/*
  Replace a vertex of the simplex.
*/

void replace (float ** simplex, float * response, 
	      int index, float * vertex, float resp)
{
  int i;

  for (i = 0;  i < DIMENSION;  i++)
    simplex[index][i] = vertex[i];

  response[index] = resp;
}


/*---------------------------------------------------------------------------*/
/*
  Calculate goodness of fit.
*/

float calc_good_fit (float * response)
{
  int i;

  float avg, sd, tmp;

  /* average the response values */
  avg = 0.0;
  for (i = 0;  i < DIMENSION+1;  i++)
    avg += response[i];
  avg /= DIMENSION+1;

  /* compute standard deviation of response */
  sd = 0.0;
  for (i = 0;  i < DIMENSION+1;  i++)
    {
      tmp = response[i] - avg;
      sd += tmp*tmp;
    }
  sd /= DIMENSION;

  return (sqrt(sd));
}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization for the Simplex algorithm.
*/

void simplex_initialize (float * parameters, float ** simplex, 
			 float * response, float * step_size)
{
  int i, j;
  int worst, next, best;
  float resp;
  float minval, maxval;


  for (j = 0;  j < DIMENSION;  j++)
    {
      simplex[0][j] = parameters[j];
      step_size[j] = 0.5 * parameters[j];
    }

  for (i = 1;  i < DIMENSION+1;  i++)
    for (j = 0;  j < DIMENSION;  j++)
      {
	minval = simplex[0][j] - step_size[j];
	maxval = simplex[0][j] + step_size[j];
	simplex[i][j] = rand_uniform (minval, maxval);
      }

  for (i = 0;  i < DIMENSION+1;  i++)
      response[i] = calc_error (simplex[i]);

  for (i = 1;  i < 500;  i++)
    {
      for (j = 0;  j < DIMENSION;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  maxval = simplex[0][j] + step_size[j];
	  parameters[j] = rand_uniform (minval, maxval);
	}

      resp = calc_error (parameters);
      eval_vertices (response, &worst, &next, &best);
      if (resp < response[worst])
	replace (simplex, response, worst, parameters, resp);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Use Simplex algorithm to estimate the voxel intensity distribution.

  The Simplex algorithm is adapted from: A Jump Start Course in C++ Programming
*/

void simplex_optimization (float * parameters, float * sse)
{
  const int MAX_ITERATIONS = 100;
  const int MAX_RESTARTS = 25;
  const float EXPANSION_COEF = 2.0;
  const float REFLECTION_COEF = 1.0;
  const float CONTRACTION_COEF = 0.5;
  const float TOLERANCE = 1.0e-10;

  float ** simplex   = NULL;
  float * centroid   = NULL;
  float * response   = NULL;
  float * step_size  = NULL;
  float * test1      = NULL;
  float * test2      = NULL;
  float resp1, resp2;
  int i, worst, best, next;
  int num_iter, num_restarts;
  int done;
  float fit;


  allocate_arrays (&simplex, &centroid, &response, &step_size, &test1, &test2);
  
  simplex_initialize (parameters, simplex, response, step_size);

  /* start loop to do simplex optimization */
  num_iter = 0;
  num_restarts = 0;
  done = 0;
  
  while (!done)
    {
      /* find the worst vertex and compute centroid of remaining simplex, 
	 discarding the worst vertex */
      eval_vertices (response, &worst, &next, &best);
      calc_centroid (simplex, worst, centroid);
      
      /* reflect the worst point through the centroid */
      calc_reflection (simplex, centroid, worst, 
		       REFLECTION_COEF, test1);
      resp1 = calc_error (test1);

      /* test the reflection against the best vertex and expand it if the
	 reflection is better.  if not, keep the reflection */
      if (resp1 < response[best])
	{
	  /* try expanding */
	  calc_reflection (simplex, centroid, worst, EXPANSION_COEF, test2);
	  resp2 = calc_error (test2);
	  if (resp2 <= resp1)    /* keep expansion */     
	    replace (simplex, response, worst, test2, resp2);
	  else                   /* keep reflection */
	    replace (simplex, response, worst, test1, resp1);
	}
      else if (resp1 < response[next])
	{
	  /* new response is between the best and next worst 
	     so keep reflection */
	  replace (simplex, response, worst, test1, resp1); 
	}
          else
	{
	  /* try contraction */
	  if (resp1 >= response[worst])
	    calc_reflection (simplex, centroid, worst, 
			     -CONTRACTION_COEF, test2);
	  else
	    calc_reflection (simplex, centroid, worst, 
			     CONTRACTION_COEF, test2);
	  resp2 =  calc_error (test2);
	  
	  /* test the contracted response against the worst response */
	  if (resp2 > response[worst])
	    {
	      /* new contracted response is worse, so decrease step size
		 and restart */
	      num_iter = 0;
	      num_restarts += 1;
	      restart (simplex, response, step_size);
	    }
	  else       /* keep contraction */
	    replace (simplex, response, worst, test2, resp2);
	}

      /* test to determine when to stop.  
	 first, check the number of iterations */
      num_iter += 1;    /* increment iteration counter */
      if (num_iter >= MAX_ITERATIONS)
	{
	  /* restart with smaller steps */
	  num_iter = 0;
	  num_restarts += 1;
	  restart (simplex, response, step_size);
	}

      /* limit the number of restarts */
      if (num_restarts == MAX_RESTARTS)  done = 1;

      /* compare relative standard deviation of vertex responses 
	 against a defined tolerance limit */
      fit = calc_good_fit (response);
      if (fit <= TOLERANCE)  done = 1;

      /* if done, copy the best solution to the output array */
      if (done) 
	{
	  eval_vertices (response, &worst, &next, &best);
	  for (i = 0;  i < DIMENSION;  i++)
	    parameters[i] = simplex[best][i];
	  *sse = response[best];
	}

    }  /* while (!done) */
 
  number_restarts = num_restarts;
  deallocate_arrays (&simplex, &centroid, &response, &step_size,
		     &test1, &test2);

}


/*---------------------------------------------------------------------------*/
