/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*--------------------------------------------------------------------------
  See mri_stats.c for the actual routines.
----------------------------------------------------------------------------*/

/****************************************************************/

float THD_stat_to_pval( float thr , int statcode , float * stataux )
{
   float pval = -1.0 ;   /* error flag */

   if( stataux == NULL && statcode != FUNC_ZT_TYPE ) return pval ;

   if( thr == 0.0 ) return 1.0 ;

   switch( statcode ){  /* if statcode is illegal, will return -1 */

      case FUNC_COR_TYPE:
         pval = correl_t2p( thr , stataux[0] , stataux[1] , stataux[2] ) ;
      break ;

      case FUNC_TT_TYPE:
         pval = student_t2p( thr , stataux[0] ) ;
      break ;

      case FUNC_FT_TYPE:
         pval = fstat_t2p( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_ZT_TYPE:               /* only type that doesn't */
         pval = normal_t2p( thr ) ;    /* use stataux parameters */
      break ;

      case FUNC_CT_TYPE:
         pval = chisq_t2p( thr , stataux[0] ) ;
      break ;

      case FUNC_BT_TYPE:
         pval = beta_t2p( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_BN_TYPE:
         pval = binomial_t2p( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_GT_TYPE:
         pval = gamma_t2p( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_PT_TYPE:
         pval = poisson_t2p( thr , stataux[0] ) ;
      break ;
   }

   return pval ;
}

/****************************************************************/

float THD_pval_to_stat( float pval , int statcode , float * stataux )
{
   float stat = -1.0 ;   /* error flag */

   if( pval >= 0.999999 ) return 0.0 ;  /* WTF */

   if( stataux == NULL && statcode != FUNC_ZT_TYPE ) return pval ;

   switch( statcode ){  /* if statcode is illegal, will return -1 */

      /** the routines below are in mri_stats.c **/

      case FUNC_COR_TYPE:
         stat = correl_p2t( pval , stataux[0] , stataux[1] , stataux[2] ) ;
      break ;

      case FUNC_TT_TYPE:
         stat = student_p2t( pval , stataux[0] ) ;
      break ;

      case FUNC_FT_TYPE:
         stat = fstat_p2t( pval , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_ZT_TYPE:                /* only type that doesn't */
         stat = normal_p2t( pval ) ;    /* use stataux parameters */
      break ;

      case FUNC_CT_TYPE:
         stat = chisq_p2t( pval , stataux[0] ) ;
      break ;

      case FUNC_BT_TYPE:
         stat = beta_p2t( pval , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_BN_TYPE:
         stat = binomial_p2t( pval , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_GT_TYPE:
         stat = gamma_p2t( pval , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_PT_TYPE:
         stat = poisson_p2t( pval , stataux[0] ) ;
      break ;
   }

   return stat ;
}

/****************************************************************/

float THD_stat_to_zscore( float thr , int statcode , float * stataux )
{
   float zscore = thr ;

   if( stataux == NULL && statcode != FUNC_ZT_TYPE ) return zscore ;

   switch( statcode ){  /* if statcode is illegal, will return -1 */

      /** the routines below are in mri_stats.c **/

      case FUNC_COR_TYPE:
         zscore = correl_t2z( thr , stataux[0] , stataux[1] , stataux[2] ) ;
      break ;

      case FUNC_TT_TYPE:
         zscore = student_t2z( thr , stataux[0] ) ;
      break ;

      case FUNC_FT_TYPE:
         zscore = fstat_t2z( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_ZT_TYPE:                 /* only type that doesn't */
         zscore = normal_t2z( thr ) ;    /* use stataux parameters */
      break ;

      case FUNC_CT_TYPE:
         zscore = chisq_t2z( thr , stataux[0] ) ;
      break ;

      case FUNC_BT_TYPE:
         zscore = beta_t2z( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_BN_TYPE:
         zscore = binomial_t2z( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_GT_TYPE:
         zscore = gamma_t2z( thr , stataux[0] , stataux[1] ) ;
      break ;

      case FUNC_PT_TYPE:
         zscore = poisson_t2z( thr , stataux[0] ) ;
      break ;
   }

   return zscore ;
}
