/*---------------------------------------------------------------------------*/
/*
  These routines estimate the probability density function (PDF) 
  corresponding to the distribution of gray-matter and white-matter
  voxel intensities.  The estimate is formed as the sum of three normal
  distributions, using the Simplex algorithm for non-linear optimization
  to estimate the unknown parameters.

  File:    estpdf.c
  Author:  B. Douglas Ward
  Date:    27 May 1999

  Mod:     Extensive restructuring of the code.
  Date:    28 January 2000
  

  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
*/


/*---------------------------------------------------------------------------*/
/*
  Include header files and forward declarations.
*/

#include "pdf.h"

float calc_error (float * vertex);


/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

#define DIMENSION 9      /* number of parameters to be estimated */

pdf p;                   /* empirical pdf */


/*---------------------------------------------------------------------------*/
/*
  Include source code files.
*/

#include "randgen.c"
#include "pdf.c"
#include "Simplex.c"


/*---------------------------------------------------------------------------*/
/*
  Perform initialization for estimation of PDF from short array. 
*/

void estpdf_short_initialize 
(
  int nxyz, 
  short * sfim,
  float * gpeak,             /* estimated peak of gray-matter distribution */
  float * wpeak              /* estimated peak of white-matter distribution */
)

{
  pdf ps;
  int gmax, wmax;
  int kk;
  int ok = 1;


  /*---- Initialize pdf's -----*/
  PDF_initialize (&p);
  PDF_initialize (&ps);


  /*----- Convert short array to pdf estimate -----*/
  PDF_short_to_pdf (nxyz, sfim, &p);
  PDF_sprint ("\nOriginal PDF:", p);


  /*----- Trim extreme values from pdf estimate -----*/
  PDF_trim (0.01, 0.99, &p);
  PDF_sprint ("\nTrimmed PDF:", p);


  /*----- Smooth the pdf estimate -----*/
  PDF_copy (p, &ps);
  PDF_smooth (&ps);
  PDF_sprint ("\nSmoothed PDF:", ps);


  /*----- Try to locate bimodality of the pdf -----*/
  ok = PDF_find_bimodal (ps, &gmax, &wmax);
  if (ok)
    {
      *gpeak = PDF_ibin_to_xvalue (ps, gmax);
      *wpeak = PDF_ibin_to_xvalue (ps, wmax);
    }
  else
    {
      printf ("Unable to find bimodal distribution \n");
      *gpeak = (2.0/3.0)*p.lower_bnd + (1.0/3.0)*p.upper_bnd;
      *wpeak = (1.0/3.0)*p.lower_bnd + (2.0/3.0)*p.upper_bnd;
    }


  printf ("\nInitial PDF estimates: \n");
  printf ("Lower Bnd = %8.3f   Upper Bnd  = %8.3f \n", 
	  p.lower_bnd, p.upper_bnd);
  printf ("Gray Peak = %8.3f   White Peak = %8.3f \n", *gpeak, *wpeak);


  PDF_destroy (&ps);

}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization for estimation of PDF from float array. 
*/

void estpdf_float_initialize 
(
  int nxyz, 
  float * ffim,
  int nbin,
  float * gpeak,             /* estimated peak of gray-matter distribution */
  float * wpeak              /* estimated peak of white-matter distribution */
)

{
  pdf ps;
  int gmax, wmax;
  int kk;
  int ok = 1;


  /*---- Initialize pdf's -----*/
  PDF_initialize (&p);
  PDF_initialize (&ps);


  /*----- Convert float array to pdf estimate -----*/
  PDF_float_to_pdf (nxyz, ffim, nbin, &p);
  PDF_sprint ("\nOriginal PDF:", p);


  /*----- Trim extreme values from pdf estimate -----*/
  PDF_trim (0.01, 0.99, &p);
  PDF_sprint ("\nTrimmed PDF:", p);


  /*----- Smooth the pdf estimate -----*/
  PDF_copy (p, &ps);
  PDF_smooth (&ps);
  PDF_sprint ("\nSmoothed PDF:", ps);


  /*----- Try to locate bimodality of the pdf -----*/
  ok = PDF_find_bimodal (ps, &gmax, &wmax);
  if (ok)
    {
      *gpeak = PDF_ibin_to_xvalue (ps, gmax);
      *wpeak = PDF_ibin_to_xvalue (ps, wmax);
    }
  else
    {
      printf ("Unable to find bimodal distribution \n");
      *gpeak = (2.0/3.0)*p.lower_bnd + (1.0/3.0)*p.upper_bnd;
      *wpeak = (1.0/3.0)*p.lower_bnd + (2.0/3.0)*p.upper_bnd;
    }


  printf ("\nInitial PDF estimates: \n");
  printf ("Lower Bnd = %8.3f   Upper Bnd  = %8.3f \n", 
	  p.lower_bnd, p.upper_bnd);
  printf ("Gray Peak = %8.3f   White Peak = %8.3f \n", *gpeak, *wpeak);


  PDF_destroy (&ps);

}


/*---------------------------------------------------------------------------*/
/*
  Generate the initial guess for the parameter vector.
*/

void generate_initial_guess (float gpeak, float wpeak, float * parameters)
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
  bmean = p.lower_bnd;

  if ((gpeak > p.lower_bnd) && (gpeak < p.upper_bnd) && (gpeak < wpeak)) 
    gmean = gpeak;
  else
    gmean = p.lower_bnd;

  if ((wpeak > p.lower_bnd) && (wpeak < p.upper_bnd) && (wpeak > gpeak))
    wmean = wpeak;
  else
    wmean = p.upper_bnd;

  if ((gmean-bmean) < 0.25*(wmean-bmean))  gmean = bmean + 0.25*(wmean-bmean);
  if ((wmean-gmean) < 0.25*(wmean-bmean))  gmean = wmean - 0.25*(wmean-bmean);


  /*----- Initialize distribution standard deviations -----*/
  bsigma = 0.25 * (p.upper_bnd - p.lower_bnd);
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

float calc_error (float * vertex)

{
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

  deltah = p.upper_bnd - p.lower_bnd;
  deltam = wmean - gmean;


  /*----- Apply constraints? -----*/
  if ((b < 0.05) || (b > 1.5))          return (BIG_NUMBER);
  if ((g < 0.05) || (g > 1.0))          return (BIG_NUMBER);
  if ((w < 0.05) || (w > 1.0))          return (BIG_NUMBER);
  if ((b+g+w < 1.0) || (b+g+w > 2.0))   return (BIG_NUMBER);

  if ((bmean < p.lower_bnd) || (bmean > p.upper_bnd))  return (BIG_NUMBER);
  if ((gmean < p.lower_bnd) || (gmean > p.upper_bnd))  return (BIG_NUMBER);
  if ((wmean < p.lower_bnd) || (wmean > p.upper_bnd))  return (BIG_NUMBER);
  if ((gmean < bmean)    || (gmean > wmean))     return (BIG_NUMBER);

  if ((gmean-bmean) < 0.10*(wmean-bmean))        return (BIG_NUMBER);
  if ((wmean-gmean) < 0.10*(wmean-bmean))        return (BIG_NUMBER);

  if ((bsigma < 0.01*deltah) || (bsigma > 0.5*deltah))  return (BIG_NUMBER);
  if ((gsigma < 0.01*deltam) || (gsigma > 0.5*deltam))  return (BIG_NUMBER);
  if ((wsigma < 0.01*deltam) || (wsigma > 0.5*deltam))  return (BIG_NUMBER);


  /*----- Not constrained, so calculate actual error sum of squares -----*/
  sse = 0.0;

  for (i = 0;  i < p.nbin;  i++)
    {
      t = PDF_ibin_to_xvalue (p, i);
      diff = p.prob[i] - estimate (vertex, t)*p.width;
      sse += diff * diff;
    }

  
  return (sse);
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
  printf ("\nrmse = %f \n", sqrt (sse / p.nbin ));

}


/*---------------------------------------------------------------------------*/
/*
  Estimate the PDF of the voxel intensities.
*/

void estpdf_short (int nxyz, short * sfim, float * parameters)
{
  float gpeak;               /* estimated peak of gray-matter distribution */
  float wpeak;               /* estimated peak of white-matter distribution */
  float sse;


  /*----- Progress report -----*/
  printf ("\nEstimating PDF of voxel intensities \n");

  
  /*----- Initialization for PDF estimation -----*/
  estpdf_short_initialize (nxyz, sfim, &gpeak, &wpeak);


  generate_initial_guess (gpeak, wpeak, parameters);
 

  /*----- Get least squares estimate for PDF parameters -----*/
  simplex_optimization (parameters, &sse);


  /*----- Report PDF parameters -----*/
  output_pdf_results (parameters, sse);


  /*----- Free memory -----*/
  /*
  PDF_destroy (&p);
  */

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the PDF of the voxel intensities.
*/

void estpdf_float (int nxyz, float * ffim, int nbin, float * parameters)
{
  float gpeak;               /* estimated peak of gray-matter distribution */
  float wpeak;               /* estimated peak of white-matter distribution */
  float sse;


  /*----- Progress report -----*/
  printf ("\nEstimating PDF of voxel intensities \n");

  
  /*----- Initialization for PDF estimation -----*/
  estpdf_float_initialize (nxyz, ffim, nbin, &gpeak, &wpeak);


  /*----- Make initial estimate of the parameters from previous results -----*/
  generate_initial_guess (gpeak, wpeak, parameters);
 

  /*----- Get least squares estimate for PDF parameters -----*/
  simplex_optimization (parameters, &sse);


  /*----- Report PDF parameters -----*/
  output_pdf_results (parameters, sse);


  /*----- Free memory -----*/
  /*
  PDF_destroy (&p);
  */
 
  return ;
}


/*---------------------------------------------------------------------------*/
