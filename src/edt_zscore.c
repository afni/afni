/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*----------------------------------------------------------------------------
   Convert an array of statistics to N(0,1) (or z-score) values.
   For t and correlation statistics, the z-scores will be signed.
   For other types, the statistic is always positive and so will the z-score.
   17 Sep 1998 -- RWCox
------------------------------------------------------------------------------*/

void EDIT_zscore_vol( int nvox ,
                      int vtype , float vfac , void * var ,
                      int statcode , float * stataux )
{
   register int ii ;

   /*-- sanity checks --*/

   if( nvox < 1                 ||                  /* no data? */
       var == NULL              ||                  /* no data? */
       ! FUNC_IS_STAT(statcode) ||                  /* not a statistic? */
       statcode == FUNC_ZT_TYPE ||                  /* already a z-score? */
       ( vtype != MRI_short && vtype != MRI_float ) /* illegal type of data? */
   ) return ;

   /*-- what type of data? --*/

   switch( vtype ){

      case MRI_float:{
         register float * bar = (float *) var ;
         register float   fac = (vfac != 0.0 ) ? vfac : 1.0 ;

         for( ii=0 ; ii < nvox ; ii++ )
            bar[ii] = THD_stat_to_zscore( fac*bar[ii] , statcode , stataux ) ;
      }
      break ;

      case MRI_short:{
         register short * bar = (short *) var ;
         register float   fac = (vfac != 0.0 ) ? vfac : 1.0 ;

         for( ii=0 ; ii < nvox ; ii++ )
            bar[ii] = (short) (  FUNC_ZT_SCALE_SHORT
                               * THD_stat_to_zscore(fac*bar[ii],statcode,stataux) ) ;
      }
      break ;
   }

   return ;
}
