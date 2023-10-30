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


/*---------------------------------------------------------------------------
 * This function is akin AFNI_set_pval(), but is meant to simply return
 * the threshold that corresponds to the input p-value, based on the dataset
 * sub-brick statistic type and DF.                     
 *
 * If as_1_sided and is a 2-sided (non-F) test, double p.
 *
 * Return -1.0 on an error.
 *                                                        [30 Oct 2023 rickr]
 */
float THD_volume_pval_to_thresh(THD_3dim_dataset * dset, int tindex,
                                float pval, int as_1_sided)
{
   float thresh;
   int   sid2, scode;

   if ( ! ISVALID_DSET(dset) )                   return -1.0;
   if ( pval <= 0.0 || pval >= 1.0 )             return -1.0;
   if ( DSET_BRICK_STATCODE(dset, tindex) <= 0 ) return -1.0;

   /* get stat code and whether it is 2-sided */
   scode = DSET_BRICK_STATCODE(dset, tindex);
   sid2  = THD_stat_is_2sided(scode , 0);

   /* If we want as_1_sided and it is a 2-sided test, double the p-value
    * (unless the stat is F, which we never double).
    * */
   if ( as_1_sided && sid2 && (scode != FUNC_FT_TYPE) )
      pval *= 2.0f;

   thresh = THD_pval_to_stat( pval, scode, DSET_BRICK_STATAUX(dset, tindex));

   if ( thresh < 0.0 ) return -1.0;
   else                return thresh;
}


float THD_stat_to_pval( float thr , int statcode , float *stataux )
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
/* pval is 2-sided tail probability for
     FUNC_COR_TYPE  FUNC_TT_TYPE  FUNC_ZT_TYPE
   is 1-sided (upper) tail probability for the others
*//**************************************************************/

float THD_pval_to_stat( float pval , int statcode , float *stataux )
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

float THD_stat_to_zscore( float thr , int statcode , float *stataux )
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

/****************************************************************/

int THD_stat_is_2sided( int statcode , int thrsign )
{
   switch( statcode ){  /* if statcode is illegal, will return -1 */

      case FUNC_COR_TYPE:
      case FUNC_TT_TYPE:
      case FUNC_ZT_TYPE: return (thrsign == 0) ? 1 : 0 ;

      case FUNC_FT_TYPE: return 1 ;  /* always 2-sided [16 Oct 2015] */

      case FUNC_PT_TYPE:
      case FUNC_GT_TYPE:
      case FUNC_BN_TYPE:
      case FUNC_BT_TYPE:
      case FUNC_CT_TYPE: return 0 ;  /* always 1-sided */
   }

   return -1 ;
}

/****************************************************************/

int THD_stat_is_signed( int statcode )
{
   switch( statcode ){  /* if statcode is illegal, will return -1 */

      case FUNC_COR_TYPE:
      case FUNC_TT_TYPE:
      case FUNC_ZT_TYPE: return 1 ;

      case FUNC_FT_TYPE: return 0 ;  /* always 2-sided [16 Oct 2015] */

      case FUNC_PT_TYPE:
      case FUNC_GT_TYPE:
      case FUNC_BN_TYPE:
      case FUNC_BT_TYPE:
      case FUNC_CT_TYPE: return 0 ;  /* always 1-sided */
   }

   return -1 ;
}
