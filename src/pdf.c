/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This file contains general purpose routines for input, manipulation, and
  output of probability density functions.

  File:    pdf.c
  Author:  B. Douglas Ward
  Date:    28 January 2000
*/

#ifndef USE_QUIET
# define quiet 0
#endif


/*---------------------------------------------------------------------------*/
/*
  Routine to print an error message and stop.
*/

void PDF_error (char * message)
{
  printf ("PDF error: %s \n", message);
  exit (1);
}


/*---------------------------------------------------------------------------*/
/*
  Initialize pdf data structure.
*/

void PDF_initialize (pdf * p)
{
  p->nbin = 0;
  p->prob = NULL;
  p->lower_bnd = 0.0;
  p->upper_bnd = 0.0;
  p->width = 0.0;

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Destroy pdf data structure by deallocating memory.
*/

void PDF_destroy (pdf * p)
{

  if (p->prob != NULL)   free (p->prob);

  PDF_initialize (p);

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Normalize pdf to have unity area.
*/

void PDF_normalize (pdf * p)
{
  int ibin;
  double sum;


  sum = 0.0;

  for (ibin = 0;  ibin < p->nbin;  ibin++)
    sum += p->prob[ibin];


  for (ibin = 0;  ibin < p->nbin;  ibin++)
    p->prob[ibin] /= sum;

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Create pdf data structure by allocating memory and initializing values.
*/

void PDF_create (int nbin, float * prob, float lower_bnd, float upper_bnd, 
		 pdf * p)
{
  int ibin;


  PDF_destroy (p);

  p->nbin = nbin;
  
  p->prob = (float *) malloc (sizeof(float) * nbin);
  PDF_MTEST (p->prob);
  for (ibin = 0;  ibin < nbin;  ibin++)
    p->prob[ibin] = prob[ibin];

  p->lower_bnd = lower_bnd;
  p->upper_bnd = upper_bnd;

  p->width = (upper_bnd - lower_bnd) / (nbin-1);

  PDF_normalize (p);
  
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Copy pdf data structure from p to pc.
*/

void PDF_copy (pdf p, pdf * pc)
{
  PDF_create (p.nbin, p.prob, p.lower_bnd, p.upper_bnd, pc);

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Convert bin number to value of independent variable at center of the bin.
*/

float PDF_ibin_to_xvalue (pdf p, int ibin)
{
  float xvalue;

  xvalue = p.lower_bnd + ibin*p.width;

  return (xvalue);
}

  
/*---------------------------------------------------------------------------*/
/*
  Convert value of independent variable to bin number.
*/

int PDF_xvalue_to_ibin (pdf p, float xvalue)
{
  int ibin;

  ibin = floor ( (xvalue - p.lower_bnd) / p.width + 0.5);

  return (ibin);
}

  
/*---------------------------------------------------------------------------*/
/*
  Convert value of independent variable to probability.
*/

float PDF_xvalue_to_pvalue (pdf p, float xvalue)
{
  int ibin;
  float pvalue;

  ibin = PDF_xvalue_to_ibin (p, xvalue);

  if ((ibin < 0) || (ibin >= p.nbin))
    pvalue = 0.0;
  else
    pvalue = p.prob[ibin];

  return (pvalue);
}

  
/*---------------------------------------------------------------------------*/
/*
  Print contents of pdf p to screen.
*/

void PDF_print (pdf p)
{
  int ibin;


  if( !quiet ){
   printf ("Number of bins = %d \n", p.nbin);
   printf ("Lower bound    = %f \n", p.lower_bnd);
   printf ("Upper bound    = %f \n", p.upper_bnd);
   printf ("Bin width      = %f \n", p.width);
  
   /*
   printf ("%3s   %10.6s   %10.6s \n", "i", "x[i]", "p[i]");
 
   for (ibin = 0;  ibin < p.nbin;  ibin++)
     printf ("%3d   %10.6f   %10.6f \n", 
	     ibin, PDF_ibin_to_xvalue(p, ibin), p.prob[ibin]);
   */
  }

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Print contents of string str and pdf p to screen.
*/

void PDF_sprint (char * str, pdf p)
{
  if( quiet ) return ;
  printf ("%s \n", str);

  PDF_print (p);  

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Write contents of pdf p to specified file.
*/

void PDF_write_file (char * filename, pdf p)
{
  int ibin;
  FILE * outfile = NULL;


  outfile = fopen (filename, "w");

  for (ibin = 0;  ibin < p.nbin;  ibin++)
    fprintf (outfile, "%d  %f  %f \n", 
	    ibin, PDF_ibin_to_xvalue(p, ibin), p.prob[ibin]);
  

  fclose (outfile);

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Simple smoothing of the pdf estimate.
*/

void PDF_smooth (pdf * p)
{
  float * sprob;
  int ibin;


  sprob = (float *) malloc (sizeof(float) * p->nbin);

  sprob[0] = 0.5*(p->prob[0] + p->prob[1]);
  sprob[p->nbin-1] = 0.5*(p->prob[p->nbin-2] + p->prob[p->nbin-1]);

  for (ibin = 1;  ibin < p->nbin-1;  ibin++)
    sprob[ibin] = 0.25 * (p->prob[ibin-1] + 2*p->prob[ibin] + p->prob[ibin+1]);
  
  free (p->prob);
  p->prob = sprob;

  PDF_normalize (p);

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Trim the pdf by removing the extreme lower and extreme upper values.
*/

void PDF_trim (float lower_per, float upper_per, pdf * p)
{
  int ibin;
  float * fbin = NULL;
  float cum_prob;
  float lower_bnd, upper_bnd;
  int lo_bin, hi_bin;


  /*----- Trim lower values -----*/
  cum_prob = 0.0;
  for (ibin = 0;  ibin < p->nbin;  ibin++)
    {
      cum_prob += p->prob[ibin];
      p->prob[ibin] = 0.0;
      if (cum_prob > lower_per)
	{
	  lo_bin = ibin + 1;
	  break;
	}
    }


  /*----- Trim upper values -----*/
  cum_prob = 0.0;
  for (ibin = p->nbin-1;  ibin >= 0;  ibin--)
    {
      cum_prob += p->prob[ibin];
      p->prob[ibin] = 0.0;
      if (cum_prob > 1.0 - upper_per)
	{
	  hi_bin = ibin - 1;
	  break;
	}
    }

  
  /*----- Reset lower and upper bounds -----*/	
  lower_bnd = PDF_ibin_to_xvalue (*p, lo_bin);
  upper_bnd = PDF_ibin_to_xvalue (*p, hi_bin);

  p->lower_bnd = lower_bnd;
  p->upper_bnd = upper_bnd;
  p->nbin = hi_bin - lo_bin + 1;


  /*----- Copy data -----*/
  fbin = (float *) malloc (sizeof(float) * p->nbin);
  for (ibin = 0;  ibin < p->nbin;  ibin++)
    fbin[ibin] = p->prob[ibin+lo_bin];


  /*----- Reset pointer to data -----*/
  free (p->prob);
  p->prob = fbin;


  /*----- Normalize to unity area -----*/
  PDF_normalize (p);


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Determine the range of values in the input short array.
*/

void PDF_short_range (int npts, short * sarray,
		       short * min_val, short * max_val)
{
  int ipt;


  *min_val = sarray[0];
  *max_val = sarray[0];

  for (ipt = 1;  ipt < npts;  ipt++)
    {
      if (sarray[ipt] < *min_val)   *min_val = sarray[ipt];
      if (sarray[ipt] > *max_val)   *max_val = sarray[ipt];
    }

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Determine the range of values in the input float array.
*/

void PDF_float_range (int npts, float * farray,
		      float * min_val, float * max_val)
{
  int ipt;


  *min_val = farray[0];
  *max_val = farray[0];

  for (ipt = 1;  ipt < npts;  ipt++)
    {
      if (farray[ipt] < *min_val)   *min_val = farray[ipt];
      if (farray[ipt] > *max_val)   *max_val = farray[ipt];
    }

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Estimate the pdf corresponding to the input short array.
*/

void PDF_short_to_pdf (int npts, short * sarray, pdf * p)
{
  const int MIN_COUNT = 5;
  const int MIN_BINS  = 5;
  int ipt, ibin, count;
  float * fbin = NULL;
  int num_bins;
  short lower_lim, upper_lim;
  char message[80];


  /*----- Make histogram of input short array -----*/
  PDF_short_range (npts, sarray, &lower_lim, &upper_lim);
  num_bins = upper_lim - lower_lim + 1;
  if (num_bins < MIN_BINS)
    {
      sprintf (message, "histogram contains only %d bins", num_bins);
      PDF_error (message);
    }

  fbin = (float *) malloc (sizeof(float) * num_bins);  PDF_MTEST (fbin);
  for (ibin = 0;  ibin < num_bins;  ibin++)
    fbin[ibin] = 0.0;
    
  count = 0;
  for (ipt = 0;  ipt < npts;  ipt++)
    {
      ibin = sarray[ipt] - lower_lim;
      if ((ibin >= 0) && (ibin < num_bins))
	{
	  fbin[ibin] += 1.0;
	  count++;
	}
    }


  /*----- Check for too few points -----*/
  if (count < MIN_COUNT)
    {
      sprintf (message, "histogram contains only %d points", count);
      PDF_error (message);
    }


  /*----- Create PDF -----*/
  PDF_create (num_bins, fbin, (float) lower_lim, (float) upper_lim, p);


  /*----- Release memory -----*/
  free (fbin);   fbin = NULL;


  return;
}
 
 
/*---------------------------------------------------------------------------*/
/*
  Estimate the pdf corresponding to the input float array.
*/
void PDF_float_to_pdf (int npts, float * farray, int num_bins, pdf * p)
{
  const int MIN_COUNT = 5;
  const int MIN_BINS  = 5;
  int ipt, ibin, count;
  float * fbin = NULL;
  float width;
  float min_val, max_val;
  char message[80];


  /*----- Make histogram of input float array -----*/
  if (num_bins < MIN_BINS)
    {
      sprintf (message, "histogram contains only %d bins", num_bins);
      PDF_error (message);
    }

  fbin = (float *) malloc (sizeof(float) * num_bins);   PDF_MTEST (fbin);
  for (ibin = 0;  ibin < num_bins;  ibin++)
    fbin[ibin] = 0.0;
  
  PDF_float_range (npts, farray, &min_val, &max_val);
  width = (max_val - min_val) / num_bins;
 
  count = 0;
  for (ipt = 0;  ipt < npts;  ipt++)
    {
      ibin = (farray[ipt] - min_val) / width;
      if ((ibin >= 0) && (ibin < num_bins))
	{
	  fbin[ibin] += 1.0;
	  count++;
	}
    }


  /*----- Check for too few points -----*/
  if (count < MIN_COUNT)
    {
      sprintf (message, "histogram contains only %d points", count);
      PDF_error (message);
    }


  /*----- Create PDF -----*/
  PDF_create (num_bins, fbin, min_val, max_val, p);


  /*----- Release memory -----*/
  free (fbin);   fbin = NULL;


  return;
}
 
 
/*---------------------------------------------------------------------------*/
/*
  Find extrema of pdf function.
*/

void PDF_find_extrema (pdf p, int * num_min, int * pdf_min, 
		       int * num_max, int * pdf_max)
{
  int ibin;
  int i;


  *num_min = 0;
  *num_max = 0;

  for (ibin = 1;  ibin < p.nbin-1;  ibin++)
    {
      if ((p.prob[ibin] < p.prob[ibin-1]) && (p.prob[ibin] < p.prob[ibin+1]))
	{
	  pdf_min[*num_min] = ibin;
	  (*num_min)++;
	}

      if ((p.prob[ibin] > p.prob[ibin-1]) && (p.prob[ibin] > p.prob[ibin+1]))
	{
	  pdf_max[*num_max] = ibin;
	  (*num_max)++;
	}
    }

  if( !quiet ){
   printf ("\nExtrema of PDF: \n");
   printf ("\nNum Local Min = %d \n", *num_min);
   for (i = 0;  i < *num_min;  i++)
     {
       ibin = pdf_min[i]; 
       printf ("x[%3d] = %8.3f   p[%3d] = %12.6f \n", 
	       ibin, PDF_ibin_to_xvalue(p, ibin), ibin, p.prob[ibin]);
     }

   printf ("\nNum Local Max = %d \n", *num_max);
   for (i = 0;  i < *num_max;  i++)
     {
       ibin = pdf_max[i]; 
       printf ("x[%3d] = %8.3f   p[%3d] = %12.6f \n", 
	       ibin, PDF_ibin_to_xvalue(p, ibin), ibin, p.prob[ibin]);
     }
  }

}


/*---------------------------------------------------------------------------*/
/*
  Find bimodality of pdf function (if possible).
*/

int PDF_find_bimodal (pdf p, int * gmax, int * wmax)
{
  const int NPTS = 12;
  int * pdf_min = NULL, * pdf_max = NULL;
  int num_min, num_max;
  int imax, temp;
  

  pdf_min = (int *) malloc (sizeof(int) * p.nbin);
  pdf_max = (int *) malloc (sizeof(int) * p.nbin);
  
  PDF_find_extrema (p, &num_min, pdf_min, &num_max, pdf_max);


  if (num_max >= 2)
    {
      if (p.prob[pdf_max[1]] >= p.prob[pdf_max[0]])
	{
	  *wmax = pdf_max[1];
	  *gmax = pdf_max[0];
	}
      else
	{
	  *wmax = pdf_max[0];
	  *gmax = pdf_max[1];
	}
      
      if (num_max > 2)
	{
	  for (imax = 2;  imax < num_max;  imax++)
	    {
	      if (p.prob[pdf_max[imax]] >= p.prob[*wmax])
		{
		  *gmax = *wmax;
		  *wmax = pdf_max[imax];
		}
	      else if (p.prob[pdf_max[imax]] >= p.prob[*gmax])
		{
		  *gmax = pdf_max[imax];
		}
	    }
	}

      if (*gmax > *wmax)
	{
	  temp = *gmax;
	  *gmax = *wmax;
	  *wmax = temp;
	}

    }  /* end if (num_max >= 2) */


  free (pdf_min);   pdf_min = NULL;
  free (pdf_max);   pdf_max = NULL;
  

  if (num_max < 2)  return (0);
  else              return (1);

}


/*---------------------------------------------------------------------------*/






