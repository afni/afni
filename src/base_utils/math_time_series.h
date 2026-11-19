#ifndef MATH_TIME_SERIES_INCLUDED
#define MATH_TIME_SERIES_INCLUDED


// calc mean and var of time series
int ts_mean_var_welford(const float *x, size_t n, double *mean, double *var, 
                        int VAR_IS_POP);

// does z-scoring transformation of a time series
int zscore_ts_welford(const float *x, size_t n, float *z, int VAR_IS_POP);


#endif
