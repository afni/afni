/*---------------------------------------------------------------------------*/
/*
  This file contains routines for performing deconvolution analysis.

  File:    Deconvolve.c
  Author:  B. Douglas Ward
  Date:    31 August 1998

  Mod:     Restructured matrix calculations to improve execution speed.
  Date:    16 December 1998

  Mod:     Report mean square error from full model.
  Date:    04 January 1999

  Mod:     Earlier termination if unable to invert X matrix.
           (Avoids redundant error messages.)
  Date:    06 January 1999

  Mod:     Modifications for matrix calculation of general linear tests.
  Date:    02 July 1999

*/

/*---------------------------------------------------------------------------*/
/*
  Include linear regression analysis software.
*/

#include "RegAna.c"


/*---------------------------------------------------------------------------*/
/* 
   Initialize independent variable X matrix 
*/

int init_indep_var_matrix 
(
  int p,                      /* number of parameters in the full model */   
  int q,                      /* number of parameters in the baseline model */ 
  int NFirst,                 /* index of first image to use in the analysis */
  int N,                      /* total number of images used in the analysis */
  int num_stimts,             /* number of stimulus time series */
  MRI_IMAGE ** stimulus,      /* stimulus time series arrays */ 
  int * min_lag,              /* minimum time delay for impulse response */
  int * max_lag,              /* maximum time delay for impulse response */
  matrix * x                  /* independent variable matrix */
)

{
  int i;                    /* time index */
  int m;                    /* X matrix column index */
  int n;                    /* X matrix row index */
  int is;                   /* input stimulus index */
  int ilag;                 /* time lag index */

  float * stim_array;       /* stimulus function time series */


  /*----- Initialize X matrix -----*/
  matrix_create (N, p, x);


  /*----- Set up columns of X matrix corresponding to  
          the baseline (null hypothesis) signal model -----*/
  for (m = 0;  m < q;  m++)
    for (n = 0;  n < N;  n++)
      {
	i = n + NFirst;
	(*x).elts[n][m] = pow ((double)i, (double)m);
      }


  /*----- Set up columns of X matrix corresponding to 
          time delayed versions of the input stimulus -----*/
  m = q;
  for (is = 0;  is < num_stimts;  is++)
    {
      if (stimulus[is]->nx < N+NFirst)
	{
	  DC_error ("Input stimulus time series is too short");
	  return (0);
	}
      stim_array = MRI_FLOAT_PTR (stimulus[is]);
      for (ilag = min_lag[is];  ilag <= max_lag[is];  ilag++)
	{
	  for (n = 0;  n < N;  n++)
	    {
	      i = n + NFirst;
	      (*x).elts[n][m] = stim_array[i-ilag];
	    }
	  m++;
	}
    }

  return (1);

}


/*---------------------------------------------------------------------------*/
/*
  Initialization for the regression analysis.
*/

int init_regression_analysis 
(
  int p,                      /* number of parameters in full model */
  int q,                      /* number of parameters in baseline model */
  int num_stimts,             /* number of stimulus time series */
  int * min_lag,              /* minimum time delay for impulse response */ 
  int * max_lag,              /* maximum time delay for impulse response */
  matrix xdata,               /* independent variable matrix */
  matrix * x_full,            /* extracted X matrix    for full model */
  matrix * xtxinv_full,       /* matrix:  1/(X'X)      for full model */
  matrix * xtxinvxt_full,     /* matrix:  (1/(X'X))X'  for full model */
  matrix * x_base,            /* extracted X matrix    for baseline model */
  matrix * xtxinvxt_base,     /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_rdcd,            /* extracted X matrices  for reduced models */
  matrix * xtxinvxt_rdcd      /* matrix:  (1/(X'X))X'  for reduced models */
)

{
  int plist[MAX_XVARS];       /* list of model parameters */
  int ip, it;                 /* parameter indices */
  int is, js;                 /* stimulus indices */ 
  int jm;                     /* lag index */
  int ok;                     /* flag for successful matrix calculation */
  matrix xtxinv_temp;         /* intermediate results */


  /*----- Initialize matrix -----*/
  matrix_initialize (&xtxinv_temp);


  /*----- Initialize matrices for the baseline model -----*/
  for (ip = 0;  ip < q;  ip++)
    plist[ip] = ip;
  ok = calc_matrices (xdata, q, plist, x_base, &xtxinv_temp, xtxinvxt_base);
  if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };


  /*----- Initialize matrices for stimulus functions -----*/
  for (is = 0;  is < num_stimts;  is++)
    {
      for (ip = 0;  ip < q;  ip++)
	{
	  plist[ip] = ip;
	}

      it = ip = q;
      
      for (js = 0;  js < num_stimts;  js++)
	{
	  for (jm = min_lag[js];  jm <= max_lag[js];  jm++)
	    {
	      if (is != js)
		{
		  plist[ip] = it;
		  ip++;
		}
	      it++;
	    }
	}

      ok = calc_matrices (xdata, p-(max_lag[is]-min_lag[is]+1), 
		     plist, &(x_rdcd[is]), &xtxinv_temp, &(xtxinvxt_rdcd[is]));
      if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };
    }


  /*----- Initialize matrices for full model -----*/
  for (ip = 0;  ip < p;  ip++)
    plist[ip] = ip;
  ok = calc_matrices (xdata, p, plist, x_full, xtxinv_full, xtxinvxt_full);
  if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };


  /*----- Destroy matrix -----*/
  matrix_destroy (&xtxinv_temp);


  return (1);
}


/*---------------------------------------------------------------------------*/
/*
  Initialization for the general linear test analysis.
*/

int init_glt_analysis 
(
  matrix xtxinv,              /* matrix:  1/(X'X)  for full model */
  int glt_num,                /* number of general linear tests */
  matrix * glt_cmat,          /* general linear test matrices */
  matrix * glt_amat           /* constant GLT matrices for later use */
)

{
  int iglt;                   /* index for general linear test */
  int ok;                     /* flag for successful matrix inversion */


  for (iglt = 0;  iglt < glt_num;  iglt++)
    {
      ok = calc_glt_matrix (xtxinv, glt_cmat[iglt], &(glt_amat[iglt]));
      if (! ok)  return (0);
    }

  return (1);
}


/*---------------------------------------------------------------------------*/
/* 
   Calculate results for this voxel. 
*/

void regression_analysis 
(
  int N,                    /* number of usable data points */
  int p,                    /* number of parameters in full model */
  int q,                    /* number of parameters in baseline model */
  int num_stimts,           /* number of stimulus time series */
  int * min_lag,            /* minimum time delay for impulse response */ 
  int * max_lag,            /* maximum time delay for impulse response */ 
  matrix x_full,            /* extracted X matrix    for full model */
  matrix xtxinv_full,       /* matrix:  1/(X'X)      for full model */
  matrix xtxinvxt_full,     /* matrix:  (1/(X'X))X'  for full model */
  matrix x_base,            /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base,     /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_rdcd,          /* extracted X matrices  for reduced models */
  matrix * xtxinvxt_rdcd,   /* matrix:  (1/(X'X))X'  for reduced models */
  vector y,                 /* vector of measured data */
  float rms_min,            /* minimum variation in data to fit full model */
  float * mse,              /* mean square error from full model */
  vector * coef_full,       /* regression parameters */
  vector * scoef_full,      /* std. devs. for regression parameters */
  vector * tcoef_full,      /* t-statistics for regression parameters */
  float * fpart,            /* partial F-statistics for the stimuli */
  float * freg,             /* regression F-statistic */
  float * rsqr,             /* coeff. of multiple determination R^2  */
  int * novar               /* flag for insufficient variation in data */
)

{
  int is;                   /* input stimulus index */
  float sse_base;           /* error sum of squares, baseline model */
  float sse_rdcd;           /* error sum of squares, reduced model */
  float sse_full;           /* error sum of squares, full model */
  vector coef_temp;         /* intermediate results */


  /*----- Initialization -----*/
  vector_initialize (&coef_temp);


  /*----- Calculate regression coefficients for baseline model -----*/
  calc_coef (xtxinvxt_base, y, &coef_temp);


  /*----- Calculate the error sum of squares for the baseline model -----*/ 
  sse_base = calc_sse (x_base, coef_temp, y);

    
  /*----- Stop here if variation about baseline is sufficiently low -----*/
  if (sqrt(sse_base/N) < rms_min)
    {
      *novar = 1;
      vector_create (p, coef_full);
      vector_create (p, scoef_full);
      vector_create (p, tcoef_full);
      for (is = 0;  is < num_stimts;  is++)
	fpart[is] = 0.0; 
      *mse = 0.0;
      *rsqr = 0.0;
      *freg = 0.0;
      vector_destroy (&coef_temp);
      return;
    }
  else
    *novar = 0;


  /*----- Calculate regression coefficients for the full model  -----*/
  calc_coef (xtxinvxt_full, y, coef_full);


  /*----- Calculate the error sum of squares for the full model -----*/ 
  sse_full = calc_sse (x_full, *coef_full, y);
  *mse = sse_full / (N-p);


  /*----- Calculate t-statistics for the regression coefficients -----*/
  calc_tcoef (N, p, sse_full, xtxinv_full, 
	      *coef_full, scoef_full, tcoef_full);

  
  /*----- Determine significance of the individual stimuli -----*/
  for (is = 0;  is < num_stimts;  is++)
    {

      /*----- Calculate regression coefficients for reduced model -----*/
      calc_coef (xtxinvxt_rdcd[is], y, &coef_temp);


      /*----- Calculate the error sum of squares for the reduced model -----*/ 
      sse_rdcd = calc_sse (x_rdcd[is], coef_temp, y);


      /*----- Calculate partial F-stat for significance of the stimulus -----*/
      fpart[is] = calc_freg (N, p, p-(max_lag[is]-min_lag[is]+1), 
			     sse_full, sse_rdcd);

    }
  

  /*----- Calculate coefficient of multiple determination R^2 -----*/
  *rsqr = calc_rsqr (sse_full, sse_base);


  /*----- Calculate the total regression F-statistic -----*/
  *freg = calc_freg (N, p, q, sse_full, sse_base);


  /*----- Dispose of vector -----*/
  vector_destroy (&coef_temp);

}
  

/*---------------------------------------------------------------------------*/
/*
  Perform the general linear test analysis for this voxel.
*/

void glt_analysis 
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  matrix x,                   /* X matrix for full model */
  vector y,                   /* vector of measured data */       
  float ssef,                 /* error sum of squares from full model */
  vector coef,                /* regression parameters for full model */
  int novar,                  /* flag for insufficient variation in data */
  int glt_num,                /* number of general linear tests */
  int * glt_rows,             /* number of linear constraints in glt */
  matrix * glt_cmat,          /* general linear test matrices */
  matrix * glt_amat,          /* constant matrices */
  vector * glt_coef,          /* linear combinations from GLT matrices */
  float * fglt                /* F-statistics for the general linear tests */
)

{
  int iglt;                   /* index for general linear test */
  int q;                      /* number of parameters in the rdcd model */
  float sser;                 /* error sum of squares, reduced model */
  vector rcoef;               /* regression parameters for reduced model */


  /*----- Initialization -----*/
  vector_initialize (&rcoef);


  /*----- Loop over multiple general linear tests -----*/
  for (iglt = 0;  iglt < glt_num;  iglt++)
    {
      /*----- Test for insufficient variation in data -----*/
      if (novar)
	{
	  vector_create (glt_rows[iglt], &glt_coef[iglt]);
	  fglt[iglt] = 0.0;
	}
      else
	{
	  /*----- Calculate the GLT linear combinations -----*/
	  calc_lcoef (glt_cmat[iglt], coef, &glt_coef[iglt]);
	  
	  /*----- Calculate regression parameters for the reduced model -----*/
	  calc_rcoef (glt_amat[iglt], coef, &rcoef);

	  /*----- Calculate error sum of squares for the reduced model -----*/
	  sser = calc_sse (x, rcoef, y);

	  /*----- Calculate the F-statistic for the reduced model -----*/
	  q = p - glt_rows[iglt]; 
	  fglt[iglt] = calc_freg (n, p, q, ssef, sser);
	}
    }


  /*----- Dispose of vector -----*/
  vector_destroy (&rcoef);

}


/*---------------------------------------------------------------------------*/

static char lbuf[4096];   /* character string containing statistical summary */
static char sbuf[256];


void report_results 
(
  int q,                      /* number of parameters in the baseline model */
  int num_stimts,             /* number of stimulus time series */
  char ** stim_label,         /* label for each stimulus */
  int * min_lag,              /* minimum time delay for impulse response */ 
  int * max_lag,              /* maximum time delay for impulse response */ 
  vector coef,                /* regression parameters */
  vector tcoef,               /* t-statistics for regression parameters */
  float * fpart,              /* partial F-statistics for the stimuli */
  float freg,                 /* total regression F-statistic */
  float rsqr,                 /* coeff. of multiple determination R^2  */
  int glt_num,                /* number of general linear tests */
  int * glt_rows,             /* number of linear constraints in glt */
  vector *  glt_coef,         /* linear combinations from GLT matrices */
  float * fglt,               /* F-statistics for the general linear tests */
  char ** label               /* statistical summary for ouput display */
)

{
  int m;                      /* coefficient index */
  int is;                     /* stimulus index */
  int ilag;                   /* time lag index */

  int iglt;                   /* general linear test index */
  int ilc;                    /* linear combination index */


  /** 22 Apr 1997: create label if desired by AFNI         **/
  /** [This is in static storage, since AFNI will copy it] **/
  
  if( label != NULL ){  /* assemble this 1 line at a time from sbuf */
    
    lbuf[0] = '\0' ;   /* make this a 0 length string to start */
    
    /** for each reference, make a string into sbuf **/


    /*----- Statistical results for baseline fit -----*/ 
    sprintf (sbuf, "\nBaseline: \n");
    strcat(lbuf,sbuf);
    for (m=0;  m < q;  m++)
      {
	sprintf (sbuf, "t^%d  coef = %10.4f    ", m, coef.elts[m]);
	strcat(lbuf,sbuf) ;
	sprintf (sbuf, "t^%d  t-stat = %10.4f\n", m, tcoef.elts[m]);
	strcat(lbuf,sbuf) ;
      }
    

    /*----- Statistical results for stimulus response -----*/
    m = q;
    for (is = 0;  is < num_stimts;  is++)
      {
	sprintf (sbuf, "\nStimulus: %s \n", stim_label[is]);
	strcat(lbuf,sbuf);
	for (ilag = min_lag[is];  ilag <= max_lag[is];  ilag++)
	  {
	    sprintf (sbuf,"h[%d] coef = %10.4f    ", ilag, coef.elts[m]);
	    strcat(lbuf,sbuf) ;
	    sprintf  (sbuf,"h[%d] t-stat = %10.4f\n", ilag, tcoef.elts[m]);
	    strcat (lbuf, sbuf);
	    m++;
	  }
	sprintf (sbuf, "%26sPartial F   = %10.4f \n", "", fpart[is]);
	strcat (lbuf, sbuf);
      }


    /*----- Statistical results for general linear test -----*/
    if (glt_num > 0)
      {
	for (iglt = 0;  iglt < glt_num;  iglt++)
	  {
	    sprintf (sbuf, "\nGeneral Linear Test #%d: \n", iglt+1);
	    strcat (lbuf,sbuf);
	    for (ilc = 0;  ilc < glt_rows[iglt];  ilc++)
	      {
		sprintf (sbuf, "LC[%d]  = %10.4f \n", 
			 ilc, glt_coef[iglt].elts[ilc]);
		strcat (lbuf,sbuf);
	      }
	    sprintf (sbuf, "F-stat = %10.4f \n", fglt[iglt]);
	    strcat (lbuf,sbuf);
	  }
      }
    

    /*----- Statistical results for full model -----*/
    sprintf (sbuf, "\nFull Model: \n");
    strcat (lbuf, sbuf);

    sprintf (sbuf, "R^2    = %10.4f \n", rsqr);
    strcat (lbuf, sbuf);
    
    sprintf (sbuf, "F-stat = %10.4f \n", freg);
    strcat (lbuf, sbuf);
    
    *label = lbuf ;  /* send address of lbuf back in what label points to */
  }
  
}


/*---------------------------------------------------------------------------*/




