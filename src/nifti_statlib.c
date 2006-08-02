/******************************/
/**** NOT FINISHED YET!!!! ****/
/******************************/

/*--------------------------------------------------------------------------*/
/*! Replacement for THD_stat_to_pval().
     - NIfTI correlation has 1 DOF param, AFNI has 3
     - AFNI p-values are 2-sided for some distributions:
       - CORREL, TTEST, ZSCORE, NORMAL, LOGISTIC, LAPLACE
       - that is, those whose domain runs from -infinity..+infinity
----------------------------------------------------------------------------*/

float NIFTI_stat_to_pval( float thr, int statcode, float *stataux )
{
   float pval = -1.0f ;

   if( thr == 0.0f ) return 1.0f ;

   return pval ;
}

/*--------------------------------------------------------------------------*/
/*! Replacement for THD_pval_to_stat().
----------------------------------------------------------------------------*/

float NIFTI_pval_to_stat( float pval, int statcode, float *stataux )
{
   float stat = -1.0f ;

   if( pval >= 0.999999f ) return 0.0f ;

   return pval ;
}

/*--------------------------------------------------------------------------*/
/*! Replacement for THD_stat_to_zscore().
----------------------------------------------------------------------------*/

float NIFTI_stat_to_zscore( float thr, int statcode, float *stataux )
{
   float zscore = thr ;

   return zscore ;
}
