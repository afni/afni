#ifndef THD_STCORR2_INCLUDED
#define THD_STCORR2_INCLUDED


typedef struct {

   char *insetA;
   char *insetB;
   char *mask_name;      
   char *prefix;          

   int out_Zcorr;

   int verb;

} PARAMS_stcorr2;

/* function to initialize params */
PARAMS_stcorr2 set_stcorr2_defaults(void);

// ---------------------------------------------------------------------------

// does z-scoring transformation of a time series
int zscore_ts_welford(const float *x, float *z, size_t n);

#endif
