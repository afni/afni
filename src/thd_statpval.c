#include "3ddata.h"
#include "thd.h"


/****************************************************************/

float THD_stat_to_pval( float thr , int statcode , float * stataux )
{
   float pval = -1.0 ;   /* error flag */

   if( stataux == NULL && statcode != FUNC_ZT_TYPE ) return pval ;

   switch( statcode ){  /* if statcode is illegal, will return -1 */

      /** the routines below in mri_stats.c, and use the cdf library **/

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
