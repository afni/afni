/*---------------------------------------------------------------------------*/
/*
  This file contains routines for performing wavelet analysis of
  time series data.

  File:    Wavelets.c
  Author:  B. Douglas Ward
  Date:    28 March 2000



  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/
/*
  Include code for fast wavelet transforms.
*/

#include "Haar.c"
#include "Daubechies.c"


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void WA_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Print time series data to screen.
*/

void ts_print (int npts, float * data)
{
  int i;


  for (i = 0;  i < npts;  i++)
    {
      printf ("%12.4f  ", data[i]);
      if (8*((i+1)/8) == i+1)  printf (" \n");
    }
      printf (" \n");

}


/*---------------------------------------------------------------------------*/
/*
  Write time series data to specified file.
*/

void ts_fprint (char * filename, int npts, float * data)
{
  int i;
  FILE * outfile = NULL;


  outfile = fopen (filename, "w");


  for (i = 0;  i < npts;  i++)
    {
      fprintf (outfile, "%f", data[i]);
      fprintf (outfile, " \n");
    }

  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate integer power of 2.
*/

int powerof2 (int n)
{
  int i, j;

  j = 1;

  if (n > 0)
    for (i = 0;  i < n;  i++)
      j *= 2;
  else if (n < 0)
    j = 0;

  return (j);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate integer log base 2.
*/

int log2 (int n)
{
  int i;
   
  i = floor (log(n)/log(2.0) + 1.0e-10);

  return (i);
}

/*---------------------------------------------------------------------------*/
/*
  Apply filter to wavelet coefficients.
*/

void FWT_1d_filter 
(
  float * filter,         /* array of filter coefficients */
  int N,                  /* log2(NPTS) */
  float * s               /* array of wavelet coefficients */ 
)

{
  int NPTS;        /* number of usable data points from input data */
  int ipts;        /* wavelet coefficient index */


  NPTS = powerof2 (N);

  for (ipts = 0;  ipts < NPTS;  ipts++)
    s[ipts] *= filter[ipts];

}


/*---------------------------------------------------------------------------*/
/*
  Set up array indicating which wavelet coefficients to set to zero.
*/

float * FWT_1d_stop_filter 
(
  int num_stop_filters,   /* number of wavelet stop filters */
  int * stop_band,        /* wavelet filter stop band */ 
  int * stop_mintr,       /* min. time for stop band */
  int * stop_maxtr,       /* max. time for stop band */
  int NFirst,             /* first image from input 3d+time dataset to use */
  int NPTS                /* number of usable data points from input data */
)

{
  int N;                       /* log2(NPTS) */
  int ipts;                    /* wavelet coefficient index */
  int band;                    /* frequency band */
  int mintr;                   /* min. value for time window (in TR) */
  int maxtr;                   /* max. value for time window (in TR) */
  int ifilter;                 /* filter index */
  float * stop_filter = NULL;  /* select wavelet coefficients to stop */


  N = log2(NPTS);
  stop_filter = (float *) malloc (sizeof(float) * NPTS);   MTEST (stop_filter);
  

  for (ipts = 0;  ipts < NPTS;  ipts++)
    {
      if (ipts == 0)
	{
	  band = -1;
	  mintr = 0;
	  maxtr = NPTS-1;
	}
      else
	{
	  band = log2(ipts);
	  mintr = (ipts - powerof2(band)) * powerof2(N-band);
	  maxtr = mintr + powerof2(N-band) - 1;
	}

      mintr += NFirst;
      maxtr += NFirst;

      stop_filter[ipts] = 1.0;
      for (ifilter = 0;  ifilter < num_stop_filters;  ifilter++)
	{
	  if (band == stop_band[ifilter])
	    {
	      if ((mintr >= stop_mintr[ifilter]) 
		  && (maxtr <= stop_maxtr[ifilter]))
		stop_filter[ipts] = 0.0;
	    }
	}

    }

  
  return (stop_filter);

}


/*---------------------------------------------------------------------------*/
/*
  Set up array indicating which wavelet coefficients to include in the model.
*/

float * FWT_1d_pass_filter
(
  int num_pass_filters,   /* number of wavelet pass filters */
  int * pass_band,        /* wavelet filter pass band */ 
  int * pass_mintr,       /* min. time for pass band */
  int * pass_maxtr,       /* max. time for pass band */
  int NFirst,             /* first image from input 3d+time dataset to use */
  int NPTS                /* number of usable data points from input data */
)

{
  int N;                       /* log2(NPTS) */
  int ipts;                    /* wavelet coefficient index */
  int band;                    /* frequency band */
  int mintr;                   /* min. value for time window (in TR) */
  int maxtr;                   /* max. value for time window (in TR) */
  int ifilter;                 /* filter index */
  float * pass_filter = NULL;  /* select wavelet coefficients to pass */


  N = log2 (NPTS);
  pass_filter = (float *) malloc (sizeof(float) * NPTS);   MTEST (pass_filter);
  

  for (ipts = 0;  ipts < NPTS;  ipts++)
    {
      if (ipts == 0)
	{
	  band = -1;
	  mintr = 0;
	  maxtr = NPTS-1;
	}
      else
	{
	  band = log2(ipts);
	  mintr = (ipts - powerof2(band)) * powerof2(N-band);
	  maxtr = mintr + powerof2(N-band) - 1;
	}

      mintr += NFirst;
      maxtr += NFirst;

      pass_filter[ipts] = 0.0;
      for (ifilter = 0;  ifilter < num_pass_filters;  ifilter++)
	{
	  if (band == pass_band[ifilter])
	    {
	      if ((mintr >= pass_mintr[ifilter]) 
		  && (maxtr <= pass_maxtr[ifilter]))
		pass_filter[ipts] = 1.0;
	    }
	}

    }

  
  return (pass_filter);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the error sum of squares.
*/

float calc_sse 
(
  int NPTS,         /* number of usable data points from input data */
  float * true,     /* actual time series data */
  float * est       /* estimated time series data */
)

{
  int ipt;          /* time point index */
  float diff;       /* difference between actual and estimated data */
  float sse;        /* estimation error sum of squares */

  sse = 0.0;
  for (ipt = 0;  ipt < NPTS;  ipt++)
    {
      diff = true[ipt] - est[ipt];
      sse += diff * diff;
    }
  
  return (sse);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the F-statistic for significance of the regression.
*/

float calc_freg
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int q,                      /* number of parameters in the rdcd model */
  float ssef,                 /* error sum of squares from full model */
  float sser                  /* error sum of squares from reduced model */
)

{
  const float MAXF = 1000.0;         /* maximum value for F-statistic */
  const float EPSILON = 1.0e-2;      /* protection against divide by zero */
  float msef;                 /* mean square error for the full model */
  float msreg;                /* mean square due to the regression */
  float freg;                 /* F-statistic for the full regression model */


  /*----- Order of reduced model = order of full model ??? -----*/
  if (p <= q)   return (0.0);


  /*----- Calculate numerator and denominator of F-statistic -----*/
  msreg = (sser - ssef) / (p - q);    if (msreg < 0.0)  msreg = 0.0;
  msef   = ssef / (n - p);            if (msef  < 0.0)  msef  = 0.0;


  if (msef < EPSILON)
    freg = 0.0;
  else
    if (msreg > MAXF*msef)  freg = MAXF;
    else                    freg = msreg / msef;


  /*----- Limit range of values for F-statistic -----*/
  if (freg < 0.0)   freg = 0.0;
  if (freg > MAXF)  freg = MAXF;


  /*----- Return F-statistic for significance of the regression -----*/
  return (freg);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the coefficient of multiple determination R^2.
*/

float calc_rsqr 
(
  float ssef,                 /* error sum of squares from full model */
  float sser                  /* error sum of squares from reduced model */
)

{
  const float EPSILON = 1.0e-2;     /* protection against divide by zero */
  float rsqr;                       /* coeff. of multiple determination R^2  */


  /*----- coefficient of multiple determination R^2 -----*/
  if (sser < EPSILON)
    rsqr = 0.0;
  else
    rsqr = (sser - ssef) / sser;


  /*----- Limit range of values for R^2 -----*/
  if (rsqr < 0.0)   rsqr = 0.0;
  if (rsqr > 1.0)   rsqr = 1.0;


  /*----- Return coefficient of multiple determination R^2 -----*/
  return (rsqr);
}


/*---------------------------------------------------------------------------*/
/*
  Perform wavelet analysis on a single input time series.
*/

void wavelet_analysis 
(
  int wavelet_type,         /* indicates type of wavelet */   
  int f,                    /* number of parameters removed by the filter */
  float * stop_filter,      /* select wavelet coefs. to stop */
  int q,                    /* number of parameters in the baseline model */
  float * base_filter,      /* select wavelet coefs. for baseline */
  int p,                    /* number of parameters in the full model */
  float * full_filter,      /* select wavelet coefs. for full model */
  int NPTS,                 /* number of usable data points from input data */
  float * ts_array,         /* array of measured data for one voxel */

  float * coef,             /* full model wavelet coefficients */
  float * sse_base,         /* baseline model error sum of squares */
  float * sse_full,         /* full model error sum of squares */
  float * ffull,            /* full model F-statistic */
  float * rfull,            /* full model R^2 stats. */

  float * coefts,           /* filtered FWT coefficients */
  float * fitts,            /* filterd time series */
  float * sgnlts,           /* signal model fitted time series */
  float * errts             /* residual error time series */
)

{
  int N;                    /* log2(NPTS) */
  int it;                   /* time point index */
  int ip;                   /* full model parameter index */
  float * filtts = NULL;    /* stop filtered time series */
  float * basefwt = NULL;   /* baseline model FWT coefficients */
  float * basets = NULL;    /* baseline model time series */
  float * fullfwt = NULL;   /* full model FWT coefficients */
  float * fullts = NULL;    /* full model time series */

    
  /*----- Initialize local variables -----*/
  N = log2(NPTS);


  /*----- Allocate memory for arrays -----*/
  filtts     = (float *) malloc (sizeof(float) * NPTS);    MTEST (filtts);
  basefwt    = (float *) malloc (sizeof(float) * NPTS);    MTEST (basefwt);
  basets     = (float *) malloc (sizeof(float) * NPTS);    MTEST (basets);
  fullfwt    = (float *) malloc (sizeof(float) * NPTS);    MTEST (fullfwt);
  fullts     = (float *) malloc (sizeof(float) * NPTS);    MTEST (fullts);


  /*----- Forward Fast Wavelet Transform -----*/
  for (it = 0;  it < NPTS;  it++)
    coefts[it] = ts_array[it];
  switch (wavelet_type)
    {
    case WA_HAAR:
      Haar_forward_FWT_1d (N, coefts);
      break;

    case WA_DAUBECHIES:
      Daubechies_forward_FWT_1d (N, coefts);
      break;
    }


  /*----- Apply stop filter to wavelet transform coefficients -----*/
  FWT_1d_filter (stop_filter, N, coefts);


  /*----- Inverse Fast Wavelet Transform of filtered FWT -----*/
  for (it = 0;  it < NPTS;  it++)
    filtts[it] = coefts[it];
  switch (wavelet_type)
    {
    case WA_HAAR:
      Haar_inverse_FWT_1d (N, filtts);
      break;

    case WA_DAUBECHIES:
      Daubechies_inverse_FWT_1d (N, filtts);
      break;
    }


  /*----- Apply baseline pass filter to wavelet transform coefficients -----*/
  for (it = 0;  it < NPTS;  it++)
    basefwt[it] = coefts[it];
  FWT_1d_filter (base_filter, N, basefwt);


 /*----- Inverse Fast Wavelet Transform of baseline FWT -----*/
  for (it = 0;  it < NPTS;  it++)
    basets[it] = basefwt[it];
  switch (wavelet_type)
    {
    case WA_HAAR:
      Haar_inverse_FWT_1d (N, basets);
      break;

    case WA_DAUBECHIES:
      Daubechies_inverse_FWT_1d (N, basets);
      break;
    }


  /*----- Calculate error sum of squares (SSE) for baseline model -----*/
  *sse_base = calc_sse (NPTS, filtts, basets);


  /*----- Apply full model filter to wavelet transform coefficients -----*/
  for (it = 0;  it < NPTS;  it++)
    fullfwt[it] = coefts[it];
  FWT_1d_filter (full_filter, N, fullfwt);


  /*----- Save full model wavelet coefficients -----*/
  ip = 0;
  for (it = 0;  it < NPTS;  it++)
    if (full_filter[it] == 1.0)
      {
	coef[ip] = fullfwt[it];
	ip++;
	if (ip >= p)  break;
      }
      

 /*----- Inverse Fast Wavelet Transform of full model FWT -----*/
  for (it = 0;  it < NPTS;  it++)
    fullts[it] = fullfwt[it];
  switch (wavelet_type)
    {
    case WA_HAAR:
      Haar_inverse_FWT_1d (N, fullts);
      break;

    case WA_DAUBECHIES:
      Daubechies_inverse_FWT_1d (N, fullts);
      break;
    }


  /*----- Calculate error sum of squares (SSE) for signal model -----*/
  *sse_full = calc_sse (NPTS, filtts, fullts);


  /*----- Calculate coefficient of multiple determination R^2 -----*/
  *rfull = calc_rsqr (*sse_full, *sse_base);


  /*----- Calculate F-statistic for significance of the signal model -----*/
  *ffull = calc_freg (NPTS-f, p, q, *sse_full, *sse_base);


  /*----- Calculate residual errors -----*/
  for (it = 0;  it < NPTS;  it++)
    {
      if (p == 0)
	errts[it] = ts_array[it] - filtts[it];
      else
	errts[it] = filtts[it] - fullts[it];
    }


  /*----- Calculate "pure" signal time series -----*/
  for (it = 0;  it < NPTS;  it++)
    sgnlts[it] = fullts[it] - basets[it];


  /*----- Save the fitted time series -----*/
  for (it = 0;  it < NPTS;  it++)
    {
      if (p == 0)
	fitts[it] = filtts[it];
      else
	fitts[it] = fullts[it];
    }


  /*----- Release memory -----*/
  free (filtts);      filtts = NULL;
  free (basefwt);     basefwt = NULL;
  free (basets);      basets = NULL;
  free (fullfwt);     fullfwt = NULL;
  free (fullts);      fullts = NULL;


  return;

}


/*---------------------------------------------------------------------------*/
/*
  Convert F-value to p-value.  
  This routine was copied from: mri_stats.c
*/


double fstat_t2p( double ff , double dofnum , double dofden )
{
   int which , status ;
   double p , q , f , dfn , dfd , bound ;

   which  = 1 ;
   p      = 0.0 ;
   q      = 0.0 ;
   f      = ff ;
   dfn    = dofnum ;
   dfd    = dofden ;

   cdff( &which , &p , &q , &f , &dfn , &dfd , &status , &bound ) ;

   if( status == 0 ) return q ;
   else              return 1.0 ;
}

/*---------------------------------------------------------------------------*/
/*
  Report the results of wavelet analysis for a single time series.
*/

static char lbuf[4096];   /* character string containing statistical summary */
static char sbuf[256];

void report_results 
(
  int N,                /* number of usable data points from input data */
  int NFirst,           /* first image from input 3d+time dataset to use */
  int f,                /* number of parameters removed by the filter */
  int p,                /* number of parameters in the full model */
  int q,                /* number of parameters in the baseline model */
  float * base_filter,  /* select wavelet coefs. for baseline */
  float * sgnl_filter,  /* select wavelet coefs. for full model */
  float * coef,         /* full model wavelet coefficients */
  float sse_base,       /* baseline model error sum of squares */
  float sse_full,       /* full model error sum of squares */
  float ffull,          /* full model F-statistic */
  float rfull,          /* full model R^2 stat. */
  char ** label         /* statistical summary for ouput display */
)

{
  int it;               /* time index */
  int icoef;            /* coefficient index */
  double pvalue;        /* p-value */
  

  /** 22 Apr 1997: create label if desired by AFNI         **/
  /** [This is in static storage, since AFNI will copy it] **/
  
  if( label != NULL ){  /* assemble this 1 line at a time from sbuf */
    
    lbuf[0] = '\0' ;   /* make this a 0 length string to start */
    
    /** for each reference, make a string into sbuf **/
    

  /*----- Display wavelet coefficients for full model -----*/
  icoef = 0;
  for (it = 0;  it < N;  it++)        
    {                      
      if (sgnl_filter[it])
	{                             
	  {
	    int band, mintr, maxtr;

	    if (it == 0)
	      {
		band = -1;
		mintr = 0;
		maxtr = N-1;
	      }
	    else
	      {
		band = log2(it);
		mintr = (it - powerof2(band)) * powerof2(log2(N)-band);
		maxtr = mintr + powerof2(log2(N)-band) - 1;
	      }
	    
	    mintr += NFirst;
	    maxtr += NFirst;
	    
	    if (base_filter[it])
	      sprintf (sbuf, "B(%2d)[%3d,%3d] = %f \n", 
		       band, mintr, maxtr, coef[icoef]);
	    else
	      sprintf (sbuf, "S(%2d)[%3d,%3d] = %f \n", 
		       band, mintr, maxtr, coef[icoef]);
	  }

	  strcat(lbuf,sbuf);

	  icoef++;
	}
	    
    }  /* End loop over Wavelet coefficients */


    /*----- Statistical results for baseline fit -----*/ 
    sprintf (sbuf, "\nBaseline: \n");
    strcat(lbuf,sbuf);
    sprintf (sbuf, "# params  = %4d \n", q);
    strcat (lbuf, sbuf);
    sprintf (sbuf, "SSE       = %10.3f \n", sse_base);
    strcat (lbuf, sbuf);
    sprintf (sbuf, "MSE       = %10.3f \n", sse_base/(N-f-q));
    strcat (lbuf, sbuf);
 

     /*----- Statistical results for full model -----*/
    sprintf (sbuf, "\nFull Model: \n");
    strcat (lbuf, sbuf);
    sprintf (sbuf, "# params  = %4d \n", p);
    strcat (lbuf, sbuf);
    sprintf (sbuf, "SSE       = %10.3f \n", sse_full);
    strcat (lbuf, sbuf);
    sprintf (sbuf, "MSE       = %10.3f \n", sse_full/(N-f-p));
    strcat (lbuf, sbuf);
    

     /*----- Summary statistics -----*/
    sprintf (sbuf, "\nSummary Statistics: \n");
    strcat (lbuf, sbuf);

    sprintf (sbuf, "R^2       = %10.3f \n", rfull);
    strcat (lbuf, sbuf);
    
    sprintf (sbuf, "F[%2d,%3d] = %10.3f \n", p-q, N-f-p, ffull);
    strcat (lbuf, sbuf);

    pvalue = fstat_t2p ( (double) ffull, (double) p-q, (double) N-f-p);
    sprintf (sbuf, "p-value   = %e  \n", pvalue);
    strcat (lbuf, sbuf);
    
    
    *label = lbuf ;  /* send address of lbuf back in what label points to */
  }
  
}


/*---------------------------------------------------------------------------*/







