/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program estimates the probability density function (PDF) 
  corresponding to the distribution of gray-matter and white-matter
  voxel intensities.  The estimate is formed as the sum of three normal
  distributions, using the Simplex algorithm for non-linear optimization
  to estimate the unknown parameters.

  The Simplex algorithm is adapted from: A Jump Start Course in C++ Programming
  
  File:    estpdf.c
  Author:  B. Douglas Ward
  Date:    27 May 1999
*/


/*---------------------------------------------------------------------------*/
/*
  Include source code.
*/

#include "randgen.c"

/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

#define DIMENSION 9      /* number of parameters to be estimated */

int * hist_data;         /* histogram of voxel gray-scale intensities */
int hist_min;            /* minimum histogram bin value;  this is set to 
			    exclude the large population near zero */
int hist_max;            /* maximum histogram bin value;  this is set to
			    exclude voxels with very high intensities */
int gpeak;               /* estimated peak of gray-matter distribution */
int wpeak;               /* estimated peak of white-matter distribution */
int numpts;              /* number of voxels within the histogram limits */

int count = 0;               /* count of sse evaluations */
int number_restarts = 0;     /* count of simplex algorithm restarts */


/*---------------------------------------------------------------------------*/
/*
  Set up the histogram and generate some of the initial parameter estimates. 

  This routine was adapted from:  program 3dstrip.c
*/

#undef MAX
#define MAX(a,b) (((a)<(b)) ? (b) : (a))

#undef MIN
#define MIN(a,b) (((a)>(b)) ? (b) : (a))

#define NPMAX 128


void find_base_value( int nxyz , short * sfim )
{
   float bper = 50.0 , bmin = 1 ;
   float tper = 98.0;

   int ii , kk , nbin , sval , sum , nbot , a,b,c , npeak,ntop , nvox ;
   int * fbin ;
   int   kmin[NPMAX] , kmax[NPMAX] ;
   int   nmin        , nmax        ;

   /*-- make histogram of shorts --*/

   fbin = (int *) malloc( sizeof(int) * 32768 ) ;

   for( kk=0 ; kk < 32768 ; kk++ ) fbin[kk] = 0 ;

   nvox = 0 ;

   for( ii=0 ; ii < nxyz ; ii++ ){
      kk = sfim[ii] ; if( kk >= 0 ){ fbin[kk]++ ; nvox++ ; }
   }

   /*-- find largest value --*/

   for( kk=32767 ; kk > 0 ; kk-- ) if( fbin[kk] > 0 ) break ;
   if( kk == 0 ){fprintf(stderr,"** find_base_value: All voxels are zero!\n");exit(1);}
   nbin = kk+1 ;

   /*-- find bper point in cumulative distribution --*/

   sval = 0.01 * bper * nvox ;
   sum  = 0 ;
   for( kk=0 ; kk < nbin ; kk++ ){
      sum += fbin[kk] ; if( sum >= sval ) break ;
   }
   nbot = kk ; if( nbot == 0 ) nbot = 1 ; if( bmin > nbot ) nbot = bmin ;
   if( nbot >= nbin-9 ){
      fprintf(stderr,"** find_base_value: Base point on histogram too high\n");
      exit(1);
   }

   hist_min = nbot;


   /*-- find tper point in cumulative distribution --*/

   sval = 0.01 * tper * nvox ;
   sum  = 0 ;
   for( kk=0 ; kk < nbin ; kk++ ){
      sum += fbin[kk] ; if( sum >= sval ) break ;
   }

   hist_max = kk ; 


   /*----- Save original histogram data -----*/
   hist_data = (int *) malloc( sizeof(int) * nbin ) ;
   for (kk = 0;  kk < nbin;  kk++)
     hist_data[kk] = fbin[kk];


   /*-- smooth histogram --*/

   b = fbin[nbot-1] ; c = fbin[nbot] ;
   for( kk=nbot ; kk < nbin ; kk++ ){
      a = b ; b = c ; c = fbin[kk+1] ; fbin[kk] = 0.25*(a+c+2*b) ;
   }

   /*-- find minima and maxima above bper point --*/

   nmin = nmax = 0 ;
   for( kk=nbot+1 ; kk < nbin ; kk++ ){
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] && nmin < NPMAX ){
         kmin[nmin++] = kk ;
      } else if( fbin[kk] > fbin[kk-1] && fbin[kk] > fbin[kk+1] && nmax < NPMAX ){
         kmax[nmax++] = kk ;
      }
   }
   

   /*-- find the largest two maxima --*/

   if( nmax == 0 ){
      fprintf(stderr,"** find_base_value: No histogram maxima above base point\n");
      exit(1);
   }

   if( nmax == 1 ){
      npeak = kmax[0] ; ntop = 0 ;
   } else {
      int f1,f2 , k1,k2 , fk , klow,kup ;

      k1 = 0 ; f1 = fbin[kmax[0]] ;
      k2 = 1 ; f2 = fbin[kmax[1]] ;
      if( f1 < f2 ){
         k1 = 1 ; f1 = fbin[kmax[1]] ;
         k2 = 0 ; f2 = fbin[kmax[0]] ;
      }

      for( kk=2 ; kk < nmax ; kk++ ){
         fk = fbin[kmax[kk]] ;
         if( fk > f1 ){
            f2 = f1 ; k2 = k1 ;
            f1 = fk ; k1 = kk ;
         } else if( fk > f2 ){
            f2 = fk ; k2 = kk ;
         }
      }
      npeak = MIN( kmax[k1] , kmax[k2] ) ;  /* smaller bin of the 2 top peaks */

      /* find valley between 2 peaks */

      ntop  = MAX( kmax[k1] , kmax[k2] ) ;

      gpeak = npeak;
      wpeak = ntop;

      
      fk = fbin[ntop] ; klow = ntop ;
      for( kk=ntop-1 ; kk >= npeak ; kk-- ){
         if( fbin[kk] < fk ){ fk = fbin[kk] ; klow = kk ; }
      }
      fk  = MAX( 0.10*fk , 0.05*fbin[ntop] ) ;
      kup = MIN( nbin-1 , ntop+3*(ntop-klow+2) ) ;
      for( kk=ntop+1 ; kk <= kup ; kk++ ) if( fbin[kk] < fk ) break ;

      ntop = kk ;
   }

   for( kk=npeak-1 ; kk > 0 ; kk-- )
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] ) break ;


   if (kk > hist_min)  hist_min = kk ;

   if( ntop == 0 )
     {
       ntop = npeak + (npeak-kk) ;
       gpeak = npeak;
       wpeak = ntop;
     }

   free (fbin);

   return ;
}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization for estimation of PDF. 
*/


Boolean estpdf_initialize ()

{
  int nx, ny, nz, nxy, nxyz, ixyz;       /* voxel counters */
  int n;                                 /* histogram bin index */
  short * sfim;                          /* pointer to anat data */


  /*----- Initialize local variables -----*/
  if (anat == NULL)  return (FALSE);
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxyz = nxy*nz;
  sfim  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  if (sfim == NULL)  return (FALSE);


  /*----- Set up histogram, and get initial parameter estimates -----*/
  find_base_value (nxyz, sfim);


  /*----- Count number of voxels inside histogram intensity limits -----*/
  numpts = 0;
  for (n = hist_min;  n < hist_max;  n++)
    numpts += hist_data[n];


  /*----- Initialize random number generator -----*/
  srand48 (1234567);


  return (TRUE);
}


/*---------------------------------------------------------------------------*/
/*
  Generate the initial guess for the parameter vector.
*/

void generate_initial_guess (float * parameters)
{
  float b;                   /* coefficient for background distribution */
  float bmean;               /* mean for background distribution */ 
  float bsigma;              /* std. dev. for background distribution */
  float g;                   /* coefficient for gray-matter distribution */
  float gmean;               /* mean for gray-matter distribution */
  float gsigma;              /* std. dev. for gray-matter distribution */
  float w;                   /* coefficient for white-matter distribution */
  float wmean;               /* mean for white-matter distribution */
  float wsigma;              /* std. dev. for white-matter distribution */


  /*----- Initialize distribution coefficients -----*/
  b = 0.75;
  g = 0.25;
  w = 0.25;


  /*----- Initialize distribution means -----*/
  bmean = hist_min;

  if ((gpeak > hist_min) && (gpeak < hist_max) && (gpeak < wpeak)) 
    gmean = gpeak;
  else
    gmean = hist_min;

  if ((wpeak > hist_min) && (wpeak < hist_max) && (wpeak > gpeak))
    wmean = wpeak;
  else
    wmean = hist_max;

  if ((gmean-bmean) < 0.25*(wmean-bmean))  gmean = bmean + 0.25*(wmean-bmean);
  if ((wmean-gmean) < 0.25*(wmean-bmean))  gmean = wmean - 0.25*(wmean-bmean);


  /*----- Initialize distribution standard deviations -----*/
  bsigma = 0.25 * (hist_max - hist_min);
  gsigma = 0.25 * (wmean - gmean);
  wsigma = 0.25 * (wmean - gmean);


  /*----- Set parameter vector -----*/
  parameters[0] = b;
  parameters[1] = bmean;
  parameters[2] = bsigma;
  parameters[3] = g;
  parameters[4] = gmean;
  parameters[5] = gsigma;
  parameters[6] = w;
  parameters[7] = wmean;
  parameters[8] = wsigma;

}


/*---------------------------------------------------------------------------*/
/*
  Write parameter vector.
*/

void write_parameter_vector (float * parameters)
{
  int i;

  printf ("Dimension = %d \n", DIMENSION);
  for (i = 0;  i < DIMENSION;  i++)
    printf ("parameter[%d] = %f \n", i, parameters[i]);
}


/*---------------------------------------------------------------------------*/
/*
  Normal probability density function.
*/

float normal (float x, float mean, float sigma)

{
  float z;

  z = (x - mean) / sigma;

  return ( (1.0/(sqrt(2.0*PI)*sigma)) * exp (-0.5 * z * z) );
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the voxel intensity distribution as the sum of three normal PDF's.
*/

float estimate (float * parameters, float x)

{
  float b, bmean, bsigma, g, gmean, gsigma, w, wmean, wsigma;
  float z, fval;


  /*----- Initialize local variables -----*/
  b      = parameters[0];
  bmean  = parameters[1];
  bsigma = parameters[2];
  g      = parameters[3];
  gmean  = parameters[4];
  gsigma = parameters[5];
  w      = parameters[6];
  wmean  = parameters[7];
  wsigma = parameters[8];


  /*----- Calculate the sum of three normal PDF's -----*/ 
  fval  = b * normal (x, bmean, bsigma);
  fval += g * normal (x, gmean, gsigma);
  fval += w * normal (x, wmean, wsigma);


  return (fval);
  
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the error sum of squares for the PDF estimate.
*/

float calc_sse (float * vertex)

{
  const float delt = 1.0;            /* histogram bin size */
  const float BIG_NUMBER = 1.0e+10;  /* return when constraints are violated */

  float b, bmean, bsigma, g, gmean, gsigma, w, wmean, wsigma;  /* parameters */
  float deltah, deltam;          /* rough estimate of spread of distribution */

  int i;
  float t;
  float diff, sse;

  count += 1;


  /*----- Assign local variables -----*/
  b      = vertex[0];
  bmean  = vertex[1];
  bsigma = vertex[2];
  g      = vertex[3];
  gmean  = vertex[4];
  gsigma = vertex[5];
  w      = vertex[6];
  wmean  = vertex[7];
  wsigma = vertex[8];

  deltah = hist_max - hist_min;
  deltam = wmean - gmean;


  /*----- Apply constraints? -----*/
  if ((b < 0.05) || (b > 1.5))          return (BIG_NUMBER);
  if ((g < 0.05) || (g > 1.0))          return (BIG_NUMBER);
  if ((w < 0.05) || (w > 1.0))          return (BIG_NUMBER);
  if ((b+g+w < 1.0) || (b+g+w > 2.0))   return (BIG_NUMBER);

  if ((bmean < hist_min) || (bmean > hist_max))  return (BIG_NUMBER);
  if ((gmean < hist_min) || (gmean > hist_max))  return (BIG_NUMBER);
  if ((wmean < hist_min) || (wmean > hist_max))  return (BIG_NUMBER);
  if ((gmean < bmean)    || (gmean > wmean))     return (BIG_NUMBER);

  if ((gmean-bmean) < 0.10*(wmean-bmean))        return (BIG_NUMBER);
  if ((wmean-gmean) < 0.10*(wmean-bmean))        return (BIG_NUMBER);

  if ((bsigma < 0.01*deltah) || (bsigma > 0.5*deltah))  return (BIG_NUMBER);
  if ((gsigma < 0.01*deltam) || (gsigma > 0.5*deltam))  return (BIG_NUMBER);
  if ((wsigma < 0.01*deltam) || (wsigma > 0.5*deltam))  return (BIG_NUMBER);


  /*----- Not constrained, so calculate actual error sum of squares -----*/
  sse = 0.0;

  for (i = hist_min;  i < hist_max;  i++)
    {
      t = i * delt;
      diff = ((float) hist_data[i])/numpts - estimate (vertex, t);
      sse += diff * diff;
    }

  
  return (sse);
}


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
    response[i] = calc_sse (simplex[i]);
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
      response[i] = calc_sse (simplex[i]);

  for (i = 1;  i < 500;  i++)
    {
      for (j = 0;  j < DIMENSION;  j++)
	{
	  minval = simplex[0][j] - step_size[j];
	  maxval = simplex[0][j] + step_size[j];
	  parameters[j] = rand_uniform (minval, maxval);
	}

      resp = calc_sse (parameters);
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
      resp1 = calc_sse (test1);

      /* test the reflection against the best vertex and expand it if the
	 reflection is better.  if not, keep the reflection */
      if (resp1 < response[best])
	{
	  /* try expanding */
	  calc_reflection (simplex, centroid, worst, EXPANSION_COEF, test2);
	  resp2 = calc_sse (test2);
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
	  resp2 =  calc_sse (test2);
	  
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
/*
  Write the parameter estimates.
*/

void output_pdf_results (float * vertex, float sse)
{
  float b, bmean, bsigma, g, gmean, gsigma, w, wmean, wsigma;


  /*----- Assign variables -----*/
  b      = vertex[0];
  bmean  = vertex[1];
  bsigma = vertex[2];
  g      = vertex[3];
  gmean  = vertex[4];
  gsigma = vertex[5];
  w      = vertex[6];
  wmean  = vertex[7];
  wsigma = vertex[8];

  printf ("hist_min = %d   hist_max = %d \n", hist_min, hist_max);
  printf ("gpeak    = %d   wpeak    = %d \n", gpeak, wpeak);
  printf ("rmse = %f \n", sqrt (sse / (hist_max - hist_min) ) );
  printf ("\nProbability Density Function Estimates: \n");
  printf ("Background Coef      = %f \n", b);
  printf ("Background Mean      = %f \n", bmean);
  printf ("Background Std Dev   = %f \n", bsigma);
  printf ("Gray Matter Coef     = %f \n", g);
  printf ("Gray Matter Mean     = %f \n", gmean);
  printf ("Gray Matter Std Dev  = %f \n", gsigma);
  printf ("White Matter Coef    = %f \n", w);
  printf ("White Matter Mean    = %f \n", wmean);
  printf ("White Matter Std Dev = %f \n", wsigma);
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the PDF of the voxel intensities.
*/

Boolean estpdf (float * parameters)
{
  float sse;
  Boolean ok;


  /*----- Progress report -----*/
  if (! quiet)  printf ("\nEstimating PDF of voxel intensities \n");

  
  /*----- Initialization for PDF estimation -----*/
  ok = estpdf_initialize ();
  if (! ok)  return (FALSE);

  generate_initial_guess (parameters);
 

  /*----- Get least squares estimate for PDF parameters -----*/
  simplex_optimization (parameters, &sse);


  /*----- Report PDF parameters -----*/
  if (! quiet)  output_pdf_results (parameters, sse);


  /*----- Free memory -----*/
  free (hist_data);    hist_data  = NULL;
  

  return (TRUE);
}


/*---------------------------------------------------------------------------*/
