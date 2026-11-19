#include <stddef.h>
#include <math.h>
#include "mrilib.h" 
#include "math_time_series.h"


/* ------------------------------------------------------------------------ 
   This is a library of basic functions for working with time series inputs.
   ------------------------------------------------------------------------ */

/*
  Calculate the mean and variance of a time series using Welford's
  algorithm.

  This algorithm is numerically efficient and stable (e.g., as opposed
  to using the simple difference of sum of squares and square of
  sums). See:
  https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  Double precision is used internally.

  if n==0 : mean and var are both 0; returns 0
  if n==1 : mean is x[0] and var is 0; returns 0

  Parameters:
  x          : (1D arr, floats) input time series
  n          : (size_t) number of elements in x
  mean       : (double) output mean value of x
  var        : (double) output variance value of x
  VAR_IS_POP : (int) if 0, use sample var factor 1/(n-1); 
  else, use population var factor 1/n

  Returns:
  out        : (int) 0 for success, nonzero for failure

*/
int ts_mean_var_welford(const float *x, size_t n, double *mean, double *var, 
                        int VAR_IS_POP)
{
   // special cases
   if (n == 0) {
      *mean = 0.0;
      *var  = 0.0;
      return 0;
   }
   else if( n == 1 ){
      *mean = (double) x[0];
      *var  = 0.0;
      return 0;
   }

   // init+declare

   size_t ii = 0;
   double M1 = 0.0, M2 = 0.0;
   double delta = 0.0, delta2 = 0.0;
   
   // Welford's online algorithm
   
   for ( ii=0 ; ii < n ; ii++ ) {
      delta = x[ii] - M1;
      M1 += delta / (ii + 1);
      
      delta2 = x[ii] - M1;
      M2 += delta * delta2;
   }
   
   // calc mean

   *mean = M1;
   
   // calc variance (we know n>0, from above)
   
   if( VAR_IS_POP ) 
      *var = M2 / n;  // population var
   else
      *var = (n > 1) ? M2 / (n-1) : 0.0; // sample var
    
   return 0;
}


/*
  Perform Z-score transform of a time series using Welford's algorithm.

  For details, see: ts_mean_var_welford()
  
  Parameters:
  x          : (1D arr, floats) input time series
  n          : (size_t) number of elements in x
  z          : (1D arr, floats) output time series, Z-scored values of x;
               must have n values
  VAR_IS_POP : (int) if 0, use sample var factor 1/(n-1); 
  else, use population var factor 1/n

  Returns:
  out        : (int) 0 for success, nonzero for failure

*/
int zscore_ts_welford(const float *x, size_t n, float *z, int VAR_IS_POP)
{
   // special cases
   if (n == 0)
      return 0;

   // init+declare

   int is_fail = 0;
   size_t ii = 0;
   double mean = 0.0, var = 0.0;
   double stdev = 0.0;

   // Calculate mean and variance with Welford

   is_fail = ts_mean_var_welford(x, n, &mean, &var, VAR_IS_POP);
    
   if( is_fail )
      return -1;

   // Handle zero variance: Z_i -> all zeros

   if (var == 0.0) {
      for ( ii = 0 ; ii < n ; ii++ )
         z[ii] = 0.0f;
      return 0;
   }

   // Compute z-scores

   stdev = sqrt(var);
   for (ii = 0; ii < n; ii++) {
      z[ii] = (float)((x[ii] - mean) / stdev);
   }
    
   return 0;
}


