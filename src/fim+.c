/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This file contains routines for implementing the fim+ functions.

  File:    fim+.c
  Author:  B. Douglas Ward
  Date:    28 April 2000


  Mod:     Use output_type array in regression_analysis routine to avoid
           some unnecessary calculations.
  Date:    18 May 2000
*/

/*---------------------------------------------------------------------------*/
/*
  Include software for linear regression analysis and sorting numbers.
*/

#include "RegAna.c"
#include "ranks.c"
#include "misc_math.h"

/*---------------------------------------------------------------------------*/
/*
  FIM output parameter definitions 
*/

#define MAX_OUTPUT_TYPE 12

static char * OUTPUT_TYPE_name[MAX_OUTPUT_TYPE] =
  { "Fit Coef", "Best Index", "% Change", "% From Ave", "Baseline", "Average", 
    "Correlation", "% From Top", "Topline", "Sigma Resid",
    "Spearman CC", "Quadrant CC" } ;


#define FIM_FitCoef      (0)
#define FIM_BestIndex    (1)
#define FIM_PrcntChange  (2)
#define FIM_Baseline     (4)
#define FIM_Correlation  (6)
#define FIM_PrcntFromAve (3)
#define FIM_Average      (5)
#define FIM_PrcntFromTop (7)
#define FIM_Topline      (8)
#define FIM_SigmaResid   (9)
#define FIM_SpearmanCC   (10)
#define FIM_QuadrantCC   (11)

/*---------------------------------------------------------------------------*/

#define USE_LEGENDRE

/* removed definition of legendre(), now in misc_math.c  22 Jun 2010 [rickr] */


/*---------------------------------------------------------------------------*/
/* 
   Initialize independent variable X matrix 
*/

int init_indep_var_matrix 
(
  int q,                      /* number of parameters in the baseline model */
  int p,                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int NFirst,                 /* index of first image to use in the analysis */
  int N,                      /* total number of images used in the analysis */
  int polort,                 /* degree of polynomial ort */
  int num_ort_files,          /* number of ort time series files */
  int num_ideal_files,        /* number of ideal time series files */
  MRI_IMAGE ** ort_array,     /* ort time series arrays */
  int ** ort_list,            /* list of ort time series */
  MRI_IMAGE ** ideal_array,   /* ideal time series arrays */
  int ** ideal_list,          /* list of ideal time series */
  float * x_bot,              /* minimum of stimulus time series */
  float * x_ave,              /* average of stimulus time series */
  float * x_top,              /* maximum of stimulus time series */
  int * good_list,            /* list of good time points */
  matrix * x                  /* independent variable matrix */
)

{
  const int BIG_NUMBER = 33333;
  int i;                    /* time index */
  int m;                    /* X matrix column index */
  int n;                    /* X matrix row index */
  int is;                   /* input ideal index */
  float * far = NULL;
  int nx, ny, iq, nq;
  int Ngood;
  matrix xgood;
#ifdef USE_LEGENDRE
  double nfac,nsub ;
#endif


  /*----- Initialize X matrix -----*/
  matrix_create (N, p, x);
  matrix_initialize (&xgood);

#ifdef USE_LEGENDRE
  nsub = 0.5*(N-1) ;
  nfac = 1.0/nsub  ;
#endif

  /*----- Set up columns of X matrix corresponding to polynomial orts -----*/
  for (m = 0;  m <= polort;  m++)
    for (n = 0;  n < N;  n++)
      {
#ifndef USE_LEGENDRE   /** the old polort way: t^m **/
	     i = n + NFirst;
	     (*x).elts[n][m] = pow ((double)i, (double)m);

#else                  /** the new polort way: Legendre P_m(t) - 29 Mar 2005 **/

	     (*x).elts[n][m] = legendre( nfac*(n-nsub) , m ) ;
#endif
      }


  /*----- Set up columns of X matrix corresponding to ort time series -----*/
  for (is = 0;  is < num_ort_files;  is++)
    {
      far = MRI_FLOAT_PTR (ort_array[is]);
      nx = ort_array[is]->nx;
      ny = ort_array[is]->ny;

      if (ort_list[is] == NULL)
	for (iq = 0;  iq < ny;  iq++)
	  {
	    for (n = 0;  n < N;  n++)
	      {
		i = n + NFirst;
		(*x).elts[n][m] = *(far + iq*nx + i);
	      }
	    m++;
	  }
      else
	{
	  nq = ort_list[is][0];
	  for (iq = 1;  iq <= nq;  iq++)
	    {
	      for (n = 0;  n < N;  n++)
		{
		  i = n + NFirst;
		  (*x).elts[n][m] = *(far + ort_list[is][iq]*nx + i);
		}
	      m++;
	    }
	}
    }


  /*----- Set up columns of X matrix corresponding to ideal time series -----*/
  for (is = 0;  is < num_ideal_files;  is++)
    {
      far = MRI_FLOAT_PTR (ideal_array[is]);
      nx = ideal_array[is]->nx;
      ny = ideal_array[is]->ny;

      if (ideal_list[is] == NULL)
	for (iq = 0;  iq < ny;  iq++)
	  {
	    for (n = 0;  n < N;  n++)
	      {
		i = n + NFirst;
		(*x).elts[n][m] = *(far + iq*nx + i);
	      }
	    
	    m++;
	  }
      else
	{
	  nq = ideal_list[is][0];
	  for (iq = 1;  iq <= nq;  iq++)
	    {
	      for (n = 0;  n < N;  n++)
		{
		  i = n + NFirst;
		  (*x).elts[n][m] = *(far + ideal_list[is][iq]*nx + i);
		}

	      m++;
	    }
	}
    }


  /*----- Remove row if ideal contains value over 33333 -----*/
  Ngood = N;
  m = 0;
  for (n = 0;  n < N;  n++)
    {
      for (is = q;  is < p;  is++)
	{
	  if ((*x).elts[n][is] >= BIG_NUMBER)  break;
	}
      if (is < p)
	{
	  Ngood--;
	}
      else
	{
	  good_list[m] = n;
	  m++;
	}
    }
  matrix_extract_rows ((*x), Ngood, good_list, &xgood);
  matrix_equate (xgood, x);


  /*----- Find min, max, and ave for each column of the X matrix -----*/
  for (is = 0;  is < p;  is++)
    {      
      x_bot[is] = x_top[is] = (*x).elts[0][is];
      x_ave[is] = 0.0;
      for (n = 0;  n < Ngood;  n++)
	{
	  if ((*x).elts[n][is] < x_bot[is])  x_bot[is] = (*x).elts[n][is];  
	  if ((*x).elts[n][is] > x_top[is])  x_top[is] = (*x).elts[n][is];
	  x_ave[is] += (*x).elts[n][is] / Ngood;
	}
    }
  
  
  matrix_destroy (&xgood);

  return (Ngood);

}


/*---------------------------------------------------------------------------*/
/*
  Initialization for the regression analysis.
*/

int init_regression_analysis 
(
  int q,                      /* number of parameters in the baseline model */
  int p,                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int N,                      /* total number of images used in the analysis */
  int num_idealts,            /* number of ideal time series */
  matrix xdata,               /* independent variable matrix */
  matrix * x_base,            /* extracted X matrix    for baseline model */
  matrix * xtxinvxt_base,     /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_ideal,           /* extracted X matrices  for ideal models */
  matrix * xtxinvxt_ideal,    /* matrix:  (1/(X'X))X'  for ideal models */
  float ** rarray             /* ranked arrays of ideal time series */
)

{
  int * plist = NULL;         /* list of model parameters */
  int ip, it;                 /* parameter indices */
  int is, js;                 /* ideal indices */ 
  int jm;                     /* lag index */
  int ok;                     /* flag for successful matrix calculation */
  matrix xtxinv_temp;         /* intermediate results */
  vector ideal;               /* ideal vector */
  vector coef_temp;           /* intermediate results */
  vector xres;                /* vector of residuals */
  float sse_base;             /* baseline model error sum of squares */ 
        

  /*----- Initialize matrix -----*/
  matrix_initialize (&xtxinv_temp);
  vector_initialize (&ideal);
  vector_initialize (&coef_temp);
  vector_initialize (&xres);


  /*----- Allocate memory -----*/
  plist = (int *) malloc (sizeof(int) * p);   MTEST (plist);


  /*----- Initialize matrices for the baseline model -----*/
  for (ip = 0;  ip < q;  ip++)
    plist[ip] = ip;
  ok = calc_matrices (xdata, q, plist, x_base, &xtxinv_temp, xtxinvxt_base);
  if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };


  /*----- Initialize matrices for ideal functions -----*/
  for (is = 0;  is < num_idealts;  is++)
    {
      for (ip = 0;  ip < q;  ip++)
	{
	  plist[ip] = ip;
	}

      plist[q] = q + is;

      ok = calc_matrices (xdata, q+1, plist, 
			  &(x_ideal[is]), &xtxinv_temp, &(xtxinvxt_ideal[is]));
      if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };
    }


  /*----- Set up the ranked array for each ideal -----*/
  for (is = 0;  is < num_idealts;  is++)
    {
      /*----- Convert column of matrix to vector -----*/
      column_to_vector (xdata, q+is, &ideal);

      /*----- Calculate regression coefficients for baseline model -----*/
      calc_coef (*xtxinvxt_base, ideal, &coef_temp);

      /*----- Calculate the error sum of squares for the baseline model -----*/
      sse_base = calc_resids (*x_base, coef_temp, ideal, &xres);
    
      /*----- Form rank array from residual array -----*/
      rarray[is] = rank_darray (N, xres.elts);

    }


  /*----- Destroy matrix -----*/
  matrix_destroy (&xtxinv_temp);
  vector_destroy (&ideal);
  vector_destroy (&coef_temp);
  vector_destroy (&xres);


  /*----- Deallocate memory -----*/
  free (plist);   plist = NULL;


  return (1);
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate a correlation coefficient. 
*/

float calc_CC
(
  int N,
  float * x,
  float * y
)

{
  const float EPSILON = 1.0e-10;   /* protect against divide by zero */
  float cc;
  int i;
  float csum, xsum, ysum, prod;


  csum = xsum = ysum = 0.0;

  for (i = 0;  i < N;  i++)
    {
      csum += x[i] * y[i];
      xsum += x[i] * x[i];
      ysum += y[i] * y[i];
    }


  prod = xsum * ysum;
  if (prod < EPSILON)
    cc = 0.0;
  else
    cc = csum / sqrt(prod);


  return (cc);
 
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate the Spearman correlation coefficient. 
*/

float calc_SpearmanCC
(
  int N,
  float * r,
  float * s
)

{
  float cc;
  float rbar;
  float * dr = NULL;
  float * ds = NULL;
  int i;

  
  dr = (float *) malloc (sizeof(float) * N);
  ds = (float *) malloc (sizeof(float) * N);


  rbar = (N+1.0) / 2.0;
  for (i = 0;  i < N;  i++)
    {
      dr[i] = r[i] - rbar;
      ds[i] = s[i] - rbar;
    }


  cc = calc_CC(N, dr, ds);

  free (dr);   dr = NULL;
  free (ds);   ds = NULL;
 
  return (cc);
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate the sign function.
*/

float sign (float x)

{
  if (x > 0.0)  return (1.0);
  if (x < 0.0)  return (-1.0);
  return (0.0);
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate the Quadrant correlation coefficient. 
*/

float calc_QuadrantCC
(
  int N,
  float * r,
  float * s
)

{
  float cc;
  float rbar;
  float * dr = NULL;
  float * ds = NULL;
  int i;

  
  dr = (float *) malloc (sizeof(float) * N);
  ds = (float *) malloc (sizeof(float) * N);


  rbar = (N+1.0) / 2.0;
  for (i = 0;  i < N;  i++)
    {
      dr[i] = sign(r[i] - rbar);
      ds[i] = sign(s[i] - rbar);
    }


  cc = calc_CC(N, dr, ds);

  free (dr);   dr = NULL;
  free (ds);   ds = NULL;
 
  return (cc);
}



/*---------------------------------------------------------------------------*/
/* 
   Calculate percentage change relative to baseline. 
*/

float percent_change (float num, float den)
{
  const float EPSILON = 1.0e-10;      /* guard against divide by zero */
  const float MAX_PERCENT = 1000.0;   /* limit maximum percent change */ 
  float PrcntChange;


  if (fabs(den) < EPSILON)
    PrcntChange = sign(num) * MAX_PERCENT;
  else
    PrcntChange  = 100.0 * num / den;
 
  if (PrcntChange >  MAX_PERCENT)   PrcntChange =  MAX_PERCENT;
  if (PrcntChange < -MAX_PERCENT)   PrcntChange = -MAX_PERCENT;

  return (PrcntChange);
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate results for this voxel. 
*/

void regression_analysis 
(
  int N,                    /* number of usable data points */
  int q,                    /* number of parameters in baseline model */
  int num_idealts,          /* number of ideal time series */
  matrix x_base,            /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base,     /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_ideal,         /* extracted X matrices  for ideal models */
  matrix * xtxinvxt_ideal,  /* matrix:  (1/(X'X))X'  for ideal models */
  vector y,                 /* vector of measured data */
  float * x_bot,            /* minimum of stimulus time series */
  float * x_ave,            /* average of stimulus time series */
  float * x_top,            /* maximum of stimulus time series */
  float ** rarray,          /* ranked arrays of ideal time series */
  int * output_type,        /* list of operator requested outputs */
  float * FimParams         /* output fim parameters */
)

{
  const float EPSILON = 1.0e-05;   /* protection against divide by zero */
  int is;                   /* input ideal index */
  float sse_base;           /* error sum of squares, baseline model */
  float sse_ideal;          /* error sum of squares, ideal model */
  vector coef_temp;         /* intermediate results */
  vector coef_best;         /* best results */
  vector yres;              /* vector of residuals */
  float rtemp, rbest;       /* best correlation coefficient */  
  float stemp, sbest;       /* best Spearman correlation coefficient */
  float qtemp, qbest;       /* best quadrant correlation coefficient */
  float mse;                /* mean square error (sample variance) */

  float * sarray = NULL;    /* ranked array of measurement data */

                            /* fim output parameters */
  float FitCoef      = 0.0; 
  int   BestIndex    = 0;
  float PrcntChange  = 0.0; 
  float Baseline     = 0.0; 
  float Correlation  = 0.0; 
  float PrcntFromAve = 0.0;
  float Average      = 0.0; 
  float PrcntFromTop = 0.0; 
  float Topline      = 0.0; 
  float SigmaResid   = 0.0; 
  float SpearmanCC   = 0.0; 
  float QuadrantCC   = 0.0;


  /*----- Initialization -----*/
  vector_initialize (&coef_temp);
  vector_initialize (&coef_best);
  vector_initialize (&yres);


  /*----- Calculate regression coefficients for baseline model -----*/
  calc_coef (xtxinvxt_base, y, &coef_temp);


  /*----- Calculate the error sum of squares for the baseline model -----*/ 
  sse_base = calc_resids (x_base, coef_temp, y, &yres);

    
  /*----- Form rank array from y array -----*/
  if (output_type[FIM_SpearmanCC] || output_type[FIM_QuadrantCC]) 
    sarray = rank_darray (N, yres.elts);


  /*----- Determine the best ideal reference for this voxel -----*/
  rbest = 0.0;  sbest = 0.0;  qbest = 0.0;  BestIndex = -1;
  for (is = 0;  is < num_idealts;  is++)
    {

      /*----- Calculate regression coefficients for ideal model -----*/
      calc_coef (xtxinvxt_ideal[is], y, &coef_temp);


      /*----- Calculate the error sum of squares for the ideal model -----*/ 
      sse_ideal = calc_sse (x_ideal[is], coef_temp, y);


      /*----- Calculate partial R^2 for this ideal -----*/
      rtemp = calc_rsqr (sse_ideal, sse_base);


      if (rtemp >= rbest)
	{
	  rbest = rtemp;
	  BestIndex = is;
	  vector_equate (coef_temp, &coef_best);
	  if (num_idealts == 1)
	    mse = sse_ideal / (N-q-1);
	  else
	    mse = sse_ideal / (N-q-2);
	}


      /*----- Calculate the Spearman rank correlation coefficient -----*/
      if (output_type[FIM_SpearmanCC])
	{ 
	  stemp = calc_SpearmanCC (N, rarray[is], sarray);
	  if (fabs(stemp) > fabs(sbest))  sbest = stemp;
	}


      /*----- Calculate the Quadrant correlation coefficient -----*/
      if (output_type[FIM_QuadrantCC])
	{ 
	  qtemp = calc_QuadrantCC (N, rarray[is], sarray);
	  if (fabs(qtemp) > fabs(qbest))  qbest = qtemp;
	}
    }
  
  if ((0 <= BestIndex) && (BestIndex < num_idealts))
    {
      float Top, Ave, Base, Center;
      int ip;
      
      Top  = x_top[q+BestIndex];  
      Ave  = x_ave[q+BestIndex];  
      Base = x_bot[q+BestIndex];
      
      
      FitCoef = coef_best.elts[q];
      Correlation = sqrt(rbest);
      if (FitCoef < 0.0)  Correlation = -Correlation;
      
      Center = 0.0;
      for (ip = 0;  ip < q;  ip++)
	Center += coef_best.elts[ip] * x_ave[ip];
      
      Baseline = Center + FitCoef*Base;
      Average  = Center + FitCoef*Ave;
      Topline  = Center + FitCoef*Top;

      
      
      PrcntChange  = percent_change (FitCoef * (Top-Base), Baseline);
      PrcntFromAve = percent_change (FitCoef * (Top-Base), Average);
      PrcntFromTop = percent_change (FitCoef * (Top-Base), Topline);
      
      SigmaResid = sqrt(mse);
      
      SpearmanCC = sbest;
      
      QuadrantCC = qbest;
    }
  

  /*----- Save output parameters -----*/
  FimParams[FIM_FitCoef]      = FitCoef;
  FimParams[FIM_BestIndex]    = BestIndex + 1.0;
  FimParams[FIM_PrcntChange]  = PrcntChange;
  FimParams[FIM_Baseline]     = Baseline;
  FimParams[FIM_Correlation]  = Correlation;
  FimParams[FIM_PrcntFromAve] = PrcntFromAve;
  FimParams[FIM_Average]      = Average;
  FimParams[FIM_PrcntFromTop] = PrcntFromTop;
  FimParams[FIM_Topline]      = Topline;
  FimParams[FIM_SigmaResid]   = SigmaResid;
  FimParams[FIM_SpearmanCC]   = SpearmanCC;
  FimParams[FIM_QuadrantCC]   = QuadrantCC;


  /*----- Dispose of vectors -----*/
  vector_destroy (&coef_temp);
  vector_destroy (&coef_best);
  vector_destroy (&yres);


  /*----- Deallocate memory -----*/
  if (sarray != NULL)
    { free (sarray);  sarray = NULL; }

}
  

/*---------------------------------------------------------------------------*/

static char lbuf[4096];   /* character string containing statistical summary */
static char sbuf[256];


void report_results 
(
  int * output_type,          /* output type options */
  float * FimParams,          /* output fim parameters */
  char ** label               /* statistical summary for ouput display */
)

{
  int ip;                     /* parameter index */

  
  if( label != NULL ){  /* assemble this 1 line at a time from sbuf */
    
    lbuf[0] = '\0' ;   /* make this a 0 length string to start */
    
    /** for each reference, make a string into sbuf **/

    for (ip = 0;  ip < MAX_OUTPUT_TYPE;  ip++)
      if (output_type[ip])
	  {
	    sprintf (sbuf, "%12s = %10.4f \n", 
		     OUTPUT_TYPE_name[ip], FimParams[ip]);
	    strcat (lbuf, sbuf);
	  }    

    
    *label = lbuf ;  /* send address of lbuf back in what label points to */
  }
  
}


/*---------------------------------------------------------------------------*/




