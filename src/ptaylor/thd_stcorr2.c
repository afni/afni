#include <stddef.h>
#include <math.h>
#include "mrilib.h" 
#include "thd_stcorr2.h"

PARAMS_stcorr2 set_stcorr2_defaults(void)
{

   PARAMS_stcorr2 defopt;

   // main input dsets: required
   defopt.insetA     = NULL;     
   defopt.insetB     = NULL;     
   defopt.mask_name  = NULL;     
   defopt.prefix     = NULL;     

   // switch about outputting Zscore value of correlation
   defopt.out_Zcorr  = 0;

   // verbosity level
   defopt.verb = 1;

   return defopt;
};

/* ------------------------------------------------------------------------ */

/*
  Z-score a time series (= 1D array of floats) using Welford's algorithm.
  
  Parameters:
  x : input array of floats
  z : output array of floats
  n : number of elements in x (and z)
 
  Uses the sample standard deviation: (n-1).  Double precision is used
  internally.  Returns 0 upon success.
 */
int zscore_ts_welford(const float *x, float *z, size_t n)
{
    if (n == 0)
        return 0;

    int ii;

    double mean = 0.0, mean2 = 0.0;
    double delta = 0.0, delta2 = 0.0;
    double variance = 0.0, stdev = 0.0;

    // Welford's online algorithm:
    // https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    for (size_t i = 0; i < n; i++) {
        delta = x[i] - mean;
        mean += delta / (i + 1);

        delta2 = x[i] - mean;
        mean2 += delta * delta2;
    }

    // Sample variance
    variance = (n > 1) ? mean2 / (n - 1) : 0.0;
    stdev = sqrt(variance);

    // Handle zero variance: all zeros
    if (stdev == 0.0) {
        for ( ii = 0 ; ii < n ; ii++ )
            z[ii] = 0.0f;
        return 0;
    }

    // Compute z-scores
    for (ii = 0; ii < n; ii++) {
        z[ii] = (float)((x[ii] - mean) / stdev);
    }
    
    return 0;
}
