/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This is the header file for software contained in Wavelets.c.

  File:    Wavelets.h
  Author:  B. Douglas Ward
  Date:    28 March 2000
*/


/*---------------------------------------------------------------------------*/

#define MAX_WAVELET_TYPE 2

static char * WAVELET_TYPE_name[MAX_WAVELET_TYPE] =
  { "Haar", "Daubechies" } ;


#define WA_HAAR          (0)
#define WA_DAUBECHIES    (1)


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void WA_error (char * message);


/*---------------------------------------------------------------------------*/
/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( WA_error ("Cannot allocate memory") )

     
/*---------------------------------------------------------------------------*/
/*
  Print time series data to screen.
*/

void ts_print (int npts, float * data);


/*---------------------------------------------------------------------------*/
/*
  Write time series data to specified file.
*/

void ts_fprint (char * filename, int npts, float * data);


/*---------------------------------------------------------------------------*/
/*
  Calculate integer power of 2.
*/

int powerof2 (int n);


/*---------------------------------------------------------------------------*/
/*
  Calculate integer log base 2.
*/

int my_log2 (int n);


/*---------------------------------------------------------------------------*/
/*
  Apply filter to wavelet coefficients.
*/

void FWT_1d_filter 
(
  float * filter,         /* array of filter coefficients */
  int N,                  /* log2(NPTS) */
  float * s               /* array of wavelet coefficients */ 
);


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
);


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
);


/*---------------------------------------------------------------------------*/
/*
  Calculate the error sum of squares.
*/

float calc_sse 
(
  int NPTS,         /* number of usable data points from input data */
  float * true,     /* actual time series data */
  float * est       /* estimated time series data */
);


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
);


/*---------------------------------------------------------------------------*/
/*
  Calculate the coefficient of multiple determination R^2.
*/

float calc_rsqr 
(
  float ssef,                 /* error sum of squares from full model */
  float sser                  /* error sum of squares from reduced model */
);


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
);


/*---------------------------------------------------------------------------*/
/*
  Report the results of wavelet analysis for a single time series.
*/

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
);


/*---------------------------------------------------------------------------*/






