/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
   Edit a THD_dataxes structure to allow for resampling to a new
   voxel size (resam).
-----------------------------------------------------------------*/

void THD_edit_dataxes( float resam , THD_dataxes * daxes ,
                                     THD_dataxes * wod_daxes )
{
   float lxx , lyy , lzz ;
   float rex , rey , rez ;

   if( ! ISVALID_DATAXES(daxes) || ! ISVALID_DATAXES(wod_daxes) ) return ;

   *wod_daxes = *daxes ;       /* copy insides, then edit them */

   if( resam <= 0.0 ) return ; /* error */

   rex = (daxes->xxdel > 0) ? resam : -resam ;  /* signed resampled */
   rey = (daxes->yydel > 0) ? resam : -resam ;  /* voxel sizes */
   rez = (daxes->zzdel > 0) ? resam : -resam ;

   lxx = daxes->nxx * daxes->xxdel ;    /* signed lengths of data box */
   lyy = daxes->nyy * daxes->yydel ;
   lzz = daxes->nzz * daxes->zzdel ;

   wod_daxes->nxx = (int)( lxx/rex + 0.499 ) ;  /* new dimensions */
   wod_daxes->nyy = (int)( lyy/rey + 0.499 ) ;  /* (will be > 0) */
   wod_daxes->nzz = (int)( lzz/rez + 0.499 ) ;

   /* go to old middle, then back out to get new edge */

   wod_daxes->xxorg = daxes->xxorg + 0.5*(lxx - daxes->xxdel)
                                   - 0.5*(wod_daxes->nxx - 1)*rex ;

   wod_daxes->yyorg = daxes->yyorg + 0.5*(lyy - daxes->yydel)
                                   - 0.5*(wod_daxes->nyy - 1)*rey ;

   wod_daxes->zzorg = daxes->zzorg + 0.5*(lzz - daxes->zzdel)
                                   - 0.5*(wod_daxes->nzz - 1)*rez ;

   /* new dimensions of the voxels */

   wod_daxes->xxdel = rex ;
   wod_daxes->yydel = rey ;
   wod_daxes->zzdel = rez ;

   /* do the bounding box thing again */

   wod_daxes->xxmin = wod_daxes->xxorg ;
   wod_daxes->xxmax = wod_daxes->xxorg + (wod_daxes->nxx-1)*wod_daxes->xxdel ;
   if( wod_daxes->xxmin > wod_daxes->xxmax ){
      float temp = wod_daxes->xxmin ;
      wod_daxes->xxmin = wod_daxes->xxmax ; wod_daxes->xxmax = temp ;
   }

   wod_daxes->yymin = wod_daxes->yyorg ;
   wod_daxes->yymax = wod_daxes->yyorg + (wod_daxes->nyy-1)*wod_daxes->yydel ;
   if( wod_daxes->yymin > wod_daxes->yymax ){
      float temp = wod_daxes->yymin ;
      wod_daxes->yymin = wod_daxes->yymax ; wod_daxes->yymax = temp ;
   }

   wod_daxes->zzmin = wod_daxes->zzorg ;
   wod_daxes->zzmax = wod_daxes->zzorg + (wod_daxes->nzz-1)*wod_daxes->zzdel ;
   if( wod_daxes->zzmin > wod_daxes->zzmax ){
      float temp = wod_daxes->zzmin ;
      wod_daxes->zzmin = wod_daxes->zzmax ; wod_daxes->zzmax = temp ;
   }

#ifdef EXTEND_BBOX
   wod_daxes->xxmin -= 0.5 * wod_daxes->xxdel ;
   wod_daxes->xxmax += 0.5 * wod_daxes->xxdel ;
   wod_daxes->yymin -= 0.5 * wod_daxes->yydel ;
   wod_daxes->yymax += 0.5 * wod_daxes->yydel ;
   wod_daxes->zzmin -= 0.5 * wod_daxes->zzdel ;
   wod_daxes->zzmax += 0.5 * wod_daxes->zzdel ;
#endif

   return ;
}
