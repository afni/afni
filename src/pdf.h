/*---------------------------------------------------------------------------*/
/*
  Header file of general purpose routines for input, manipulation, and
  output of probability density functions.

  File:    pdf.h
  Author:  B. Douglas Ward
  Date:    28 January 2000

  
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
*/


/*---------------------------------------------------------------------------*/
/*
  Define pdf data structure.
*/


typedef struct pdf
{
  int nbin;
  float * prob;
  float lower_bnd;
  float upper_bnd;
  float width;
} pdf;


/*---------------------------------------------------------------------------*/
/*
  Routine to print an error message and stop.
*/

void PDF_error (char * message);


/*---------------------------------------------------------------------------*/
/*
  macro to test a malloc-ed pointer for validity 
*/

#define PDF_MTEST(ptr) \
if((ptr)==NULL) \
( PDF_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Initialize pdf data structure.
*/

void PDF_initialize (pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Destroy pdf data structure by deallocating memory.
*/

void PDF_destroy (pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Normalize pdf to have unity area.
*/

void PDF_normalize (pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Create pdf data structure by allocating memory and initializing values.
*/

void PDF_create (int nbin, float * prob, float lower_bnd, float upper_bnd, 
		 pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Copy pdf data structure from p to pc.
*/

void PDF_copy (pdf p, pdf * pc);


/*---------------------------------------------------------------------------*/
/*
  Convert bin number to value of independent variable at center of the bin.
*/

float PDF_ibin_to_xvalue (pdf p, int ibin);

  
/*---------------------------------------------------------------------------*/
/*
  Convert value of independent variable to bin number.
*/

int PDF_xvalue_to_ibin (pdf p, float xvalue);

  
/*---------------------------------------------------------------------------*/
/*
  Convert value of independent variable to probability.
*/

float PDF_xvalue_to_pvalue (pdf p, float xvalue);

  
/*---------------------------------------------------------------------------*/
/*
  Print contents of pdf p to screen.
*/

void PDF_print (pdf p);


/*---------------------------------------------------------------------------*/
/*
  Print contents of string str and pdf p to screen.
*/

void PDF_sprint (char * str, pdf p);


/*---------------------------------------------------------------------------*/
/*
  Write contents of pdf p to specified file.
*/

void PDF_write_file (char * filename, pdf p);


/*---------------------------------------------------------------------------*/
/*
  Simple smoothing of the pdf estimate.
*/

void PDF_smooth (pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Trim the pdf by removing the extreme lower and extreme upper values.
*/

void PDF_trim (float lower_per, float upper_per, pdf * p);


/*---------------------------------------------------------------------------*/
/*
  Determine the range of values in the input short array.
*/

void PDF_short_range (int npts, short * sarray,
		       short * min_val, short * max_val);


/*---------------------------------------------------------------------------*/
/*
  Determine the range of values in the input float array.
*/

void PDF_float_range (int npts, float * farray,
		      float * min_val, float * max_val);


/*---------------------------------------------------------------------------*/
/*
  Estimate the pdf corresponding to the input short array.
*/

void PDF_short_to_pdf (int npts, short * sarray, pdf * p);
 
 
/*---------------------------------------------------------------------------*/
/*
  Estimate the pdf corresponding to the input float array.
*/

void PDF_float_to_pdf (int npts, float * farray, int num_bins, pdf * p);
 
 
/*---------------------------------------------------------------------------*/
/*
  Find extrema of pdf function.
*/

void PDF_find_extrema (pdf p, int * num_min, int * pdf_min, 
		       int * num_max, int * pdf_max);


/*---------------------------------------------------------------------------*/
/*
  Find bimodality of pdf function (if possible).
*/

int PDF_find_bimodal (pdf p, int * gmax, int * wmax);


/*---------------------------------------------------------------------------*/






