#define OMIT_MAIN
#include "nifti_stats.c"

/*--------------------------------------------------------------------------*/
/*! Replacement for THD_stat_to_pval. */

float NIFTI_stat_to_pval( float thr, int statcode, float *stataux )
{
   float pval = -1.0f ;

   if( thr == 0.0f ) return 1.0f ;

   return pval ;
}

/*--------------------------------------------------------------------------*/

float NIFTI_pval_to_stat( float pval, int statcode, float *stataux )
{
   float stat = -1.0f ;

   if( pval >= 0.999999f ) return 0.0f ;

   return pval ;
}

/*--------------------------------------------------------------------------*/

float NIFTI_stat_to_zscore( float thr, int statcode, float *stataux )
{
   float zscore = thr ;

   return zscore ;
}
